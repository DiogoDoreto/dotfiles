# Trusted Nix builds through Forgejo Actions

Status: implemented locally, **disabled pending manual security audit and rollout**.
Replaces the former `nix-host-updater` standalone orchestrator (removed).
Harmonia and its signing/client configuration are unchanged; see [nix-cache.md](nix-cache.md).

## Ownership

`.forgejo/workflows/nix-build.yml` owns checkout, checking, building, and the
per-host result. Forgejo owns logs, status, cancellation, retry and run history.
PR open/reopen/head updates, main pushes, and manual dispatch each produce three
independent matrix jobs (`fail-fast: false`), queued by runner capacity 1. No job
depends on another. Mini uses `nixosConfigurations.dogdot`; Lapdog and Chungus
use their own names. All three always build, without path filters.

Checkout uses the PR source `head.sha`, otherwise the event SHA (pushed main
commit or selected dispatch revision), never a synthetic merge. The local
checkout action mirror is pinned to v4.4.0's commit, uses node20, retains the
whole repository and history, and does not persist checkout credentials.
Expressions retain Forgejo's documented `github` context alias. The pinned native
runner supports `github`, `forgejo` and `forge` as aliases of the same context;
checkout and `$GITHUB_OUTPUT` remain compatible, so YAML needs no migration.
Branch/commit inputs travel through environment variables, not shell source
interpolation. Both check and build forbid lock updates/writes. A merge can
produce a different store path than its PR head; wait for main's actual build
before deploying. Nothing activates a NixOS system generation.

## Mandatory security gate

Host jobs execute arbitrary repository code with the runner's identity. Nix's
build sandbox is **not** isolation for workflows, actions, or evaluation. Anyone
allowed to run code here can persist code as that user and populate Harmonia's
served store. Neither the root helper nor editable YAML is a trust boundary.

Before setting `dog.services.nix-build-runner.enable = true` on Chungus:

1. Audit `/diogo/dotfiles` access: only trusted users may push branches, dispatch
   workflows, administer Actions, approve workflows, or hold repository/Actions
   write permissions. Check instance administrators and organization teams too.
2. Verify the **existing UI-created runner is repository-scoped**, from
   `https://git.local.doreto.com.br/diogo/dotfiles/settings/actions/runners`.
   UUID: `3cec0ee6-b681-46ae-bb8d-eba9b6eec76c`. The user created it and saved
   its authentication secret on Chungus; scope still needs operator verification.
   Do not create a second runner, run legacy registration, use Mini's instance
   registration token, or use user/org/instance scope. Only this runner may
   advertise `chungus-nix`; Mini advertises `nix` and Ubuntu labels, so cannot
   accept these jobs.
3. Verify Forgejo's server-side PR trust gate on the deployed version. In 16.0.6,
   read-only fork/AGit authors need approval. Remove any permanent approvals for
   untrusted authors; **never approve fork/untrusted workflows on this runner**,
   not even “once”. Elevated users bypass approvals even from forks, so all such
   users must follow a trusted same-repository-branches-only policy. Do not use
   AGit to bypass that policy. A trusted author is responsible for every pusher
   to their source branch. Keep registration restricted as currently configured.
4. Test a read-only fork whose YAML removes all conditions and requests
   `chungus-nix`: it must remain blocked server-side, including after further
   pushes and actions by maintainers. Deny its trust request; do not execute it
   to test the block. Verify trusted same-repo PRs run without manual approval.

Forgejo 16 has no documented per-runner “same-repo PR authors only” allowlist.
Repository scope does not exclude fork PR events; labels are routing, not ACLs.
The YAML fork condition is only defense in depth. If the access/approval policy
cannot be enforced and verified, **leave this host runner disabled** and use a
separately designed isolated runner instead. Never use `pull_request_target` to
execute source-head code. No administrator/push/API/signing secrets are supplied
to builds. Forgejo still supplies its automatic job token; `pull_request` tokens
are read-only, while main/manual tokens have Forgejo's normal permissions. Do
not assume a GitHub-style `permissions:` block reduces those permissions here.

Promotion uses that same short-lived automatic token for its fixed-repository
HTTPS Git read (anonymous `/diogo/dotfiles` API access returned 404, which does
not distinguish private from absent). Only the promotion step exposes it as
`NIX_CACHE_JOB_TOKEN`. The helper sends a repository-scoped HTTP authorization
header via Git's process environment, disables redirects, and never persists it
or places it in argv. Nix subprocesses do not receive it. No additional read or
updater credential is provisioned; authenticated ref access remains a pilot check.

## Runner identity and manual rollout

`hosts/chungus/services/forgejo-runner.nix` uses the pinned nixpkgs
`services.forgejo-runner` module and native Forgejo Runner **13.2.0** (not Mini's
unchanged legacy integration). Instance key `chungus` yields unit
`forgejo-runner-chungus` and state/HOME `/var/lib/forgejo-runner/chungus`.
The upstream module defaults to `DynamicUser=true`; this integration explicitly
overrides **both** user and group to static system account `chungus-nix` and
disables DynamicUser. It disallows other native or legacy runner instances on Chungus.

`settings.server.connections.default` contains the server URL and the existing
UI UUID. `secrets.server.connections.default.token_url` points to the existing
secret file. The module emits `LoadCredential` and a non-secret
`file:$CREDENTIALS_DIRECTORY/server__connections__default__token_url` reference
in store-resident YAML. The runner resolves that reference itself; neither the
token nor a rendered secret config enters the Nix store, logs or argv. There is
no registration/re-registration hook or generated `.runner` credential file.
The UI owns the runner name; the local settings own labels and capacity.

The account has no login, supplementary groups, sudo/activation access, or Nix
trusted-user entry. The host label does not request Podman/Docker groups or a
socket, despite Podman being enabled on Chungus. The service hides Docker/Podman
sockets, `/etc/secrets` and home directories, uses a read-only system namespace,
and can write its state and `/var/lib/chungus-nix-roots` (0700). Ordinary Nix
daemon access remains available. The existing Chungus CA trust includes Mini's
Caddy CA; TLS verification is never disabled.

After the security audit, the operator manually:

1. Adds `dog.services.nix-build-runner.enable = true` to Chungus's configuration.
2. Verifies `/etc/secrets/forgejo/chungus-nix-token` is root-owned, mode 0600,
   containing the **existing UI runner's raw authentication secret**, not
   `TOKEN=...` or a legacy registration token. It is already provisioned per the
   user; do not print it or provision a replacement for rollout. Starting the
   daemon authenticates the existing UUID; it does not register another runner.
   Host jobs share the runner identity and can read its systemd credential:
   trust every job and rotate/revoke this runner's authentication if compromised.
   This is not an updater token. Secret rotation requires a service restart to
   reload systemd credentials; label changes require no registration.
3. Formats/checks and manually deploys Chungus; the agent does not activate it.
4. Inspects `systemctl cat forgejo-runner-chungus`, `id chungus-nix`, actual Nix
   trusted users and socket/secret permissions. Confirms only the repository's
   runner list shows it, with the unique label and capacity 1. Repeat the
   server-side approval test before allowing real jobs.
5. Dispatches main. Checks exact checkout SHA, three visible host results,
   persistent roots, and a cache download on another host. Opens a trusted PR
   which fails one host: the other two must still run/promote. Tests cancellation
   and retry through Forgejo. Inspect runner startup with
   `journalctl -u forgejo-runner-chungus`; build logs live in Actions.

No agent registration, secret read/provisioning, server settings mutation, full
host build, or deployment was performed. The user supplied the existing UUID and
reported saving its secret; neither implies authorization to enable the service.
Live runner/server compatibility, status association, independent matrix failures, and namespace
permissions remain rollout checks, not claimed results.

## Small retention interface

The service-local Python command is installed on the runner PATH, with no new
derivation flake. It does not checkout, check, build, or keep job logs/history.

```sh
staging_root="$(nix-cache-roots stage)"
# Actions runs nix build ... --out-link "$staging_root" before promotion.
# Promotion runs in Actions with NIX_CACHE_JOB_TOKEN set to github.token.
nix-cache-roots promote --branch main --host mini --commit "$built_sha" \
  --staging-root "$staging_root"
```

Filesystem source of truth:

```text
/var/lib/chungus-nix-roots/
  promotion.lock
  staging/<random-id>/result -> /nix/store/<system>
  staging/<random-id>/provenance.json
  branches/<sha256-of-full-ref>/<host> -> .../staging/<random-id>
```

`stage` only allocates a path. **Nix build registers** that persistent out-link
before exiting, outside the ephemeral checkout. Nix's indirect auto GC root
names that exact out-link; moving/renaming it would break registration. Promotion
never moves it. Instead, it validates the store target and any previous selected
root against its immutable provenance, queries the fixed authoritative HTTPS
remote `/diogo/dotfiles` with `git ls-remote`, requires exact source-ref/SHA
equality, then atomically replaces the branch pointer on the same filesystem.
Provenance is written once before publication, not replicated in a manifest.
Allocation fsyncs the staging parent; promotion persists staging contents and
both staging/branch-ref parent entries before publication. The pointer directory
is fsynced before unlinking the previous registered link. Old-root queries happen
before final new-root validation; validation rechecks symlink/provenance after
blocking store queries. A detected post-publication mismatch restores the prior
pointer (or removes a first-ever pointer), retaining both registered links.
The short flock includes validation/query/promotion/old-link release, not builds.
Any future manual reconciliation must use this same lock.

A failed check/build never promotes. Stale/deleted refs, remote errors and corrupt
existing roots fail closed; previous success remains. Old selected roots are
released only after a successful replacement; unrelated staging roots are not
removed. No status database labels an old root current: inspect its provenance
and compare with Forgejo's current branch tip and Actions result.

There is inevitably a race between the remote observation and local atomic
promotion (and a force-push back to the same SHA is indistinguishable). Capacity 1
serializes builds; future multi-runner automation needs a fresh concurrency
design. Crashes/cancellation can leave abandoned registered staging links,
including an old success after pointer replacement. These are conservatively
retained; there is **no recovery engine or automatic abandoned-root cleanup**.
Do not move staging links or sweep that directory. Deleted-branch reconciliation,
cleanup tooling and lock-update/PR creation are deferred. Closing/merging/deleting
a PR or branch does not remove roots in this slice. No timer or GC policy changes.

## Verification and primary references

```sh
python3 -m unittest discover -s scripts/nix-cache-roots/tests -v
NIX_CACHE_ROOTS_REAL_NIX=1 python3 -m unittest discover -s scripts/nix-cache-roots/tests -v
make format-nix
nix flake check ./hosts/lapdog/
nix flake check ./hosts/mini/
nix flake check ./hosts/chungus/ --no-build
```

Real GC tests use tiny derivations in an explicit disposable `local?store=...`
under `.private/tmp`, never the production store. They demonstrate retention,
replacement, failed builds, checkout independence and broken moved-indirect-root
semantics, plus real local Git stale/deleted/error cases and corrupt targets.
Corruption regressions cover link changes during remote lookup, old-root
validation and post-publication store validation. Ordering tests require parent
directory fsyncs before publication and previous-root release.

Verified against the deployed server's read-only `/api/v1/version` (16.0.6),
[Forgejo 16 workflow/event/matrix/context reference](https://forgejo.org/docs/v16.0/user/actions/reference/),
[PR trust rules](https://forgejo.org/docs/v16.0/user/actions/security-pull-request/),
[host runner risks](https://forgejo.org/docs/v16.0/user/actions/security/),
and the local `actions/checkout` mirror's `action.yml` / `src/ref-helper.ts` at
`11d5960a326750d5838078e36cf38b85af677262` (explicit SHA fetch/checkout, node20,
fetch-depth, persist-credentials). The actual Chungus-pinned nixpkgs module
`nixos/modules/services/continuous-integration/forgejo-runner.nix`, package
`pkgs/by-name/fo/forgejo-runner/package.nix` (13.2.0), and runner
`internal/pkg/config/config.example.yaml` / `config.go` confirm unit naming,
identities, state directories, credential URL expansion, host-label group
behavior and capacity. See also [Forgejo 16 UI UUID/token registration](https://forgejo.org/docs/v16.0/admin/actions/registration/).
Its `act/model/workflow.go` implements matrix `fail-fast: false`, and
`act/exprparser/interpreter.go` confirms all three context aliases.

Native-runner correction verification: all three flake checks passed (Chungus
`--no-build`), formatting passed, and all 24 isolated-store helper tests passed.
Enabled-module evaluation has no failed assertions, root-only Nix trusted users,
static user/group and no container supplementary groups. Only the runner package
and generated YAML/unit were realized, not a system. Runner `generate-config`,
`validate --workflow --path .forgejo/workflows/nix-build.yml` and `exec --list`
passed offline. The exact generated config also loaded with a dummy credential
in a network-isolated bubblewrap namespace: daemon startup reached its connection
attempt, which failed as expected with network unreachable. No real secret or
network registration was used. Live TLS/checkout/token permissions, repository
scope and independent matrix status/cancellation remain manual pilot checks.
