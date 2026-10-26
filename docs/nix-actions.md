# Trusted Nix builds through Forgejo Actions

Status: user-deployed and live-tested; implementation remains disabled by default.
The operator accepted the security audit for the private, trusted-only instance.
Test the fork-approval gate before admitting untrusted users. Manual reconciliation
and on-demand maintenance are implemented locally; production cleanup has not
been performed.
Replaces the former `nix-host-updater` standalone orchestrator (removed).
The manual input updater and best-effort PR host package change reports are now
implemented locally; their separate bot credential/variable, Python PATH rollout
and live pilot remain operator work. See [nix-input-updates.md](nix-input-updates.md)
for approved behavior, verification results and primary-source compatibility.
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
   UUID: `8e6aeca1-88bd-4333-9d8c-f382648ca963`. The user created it and saved
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
to Nix check/build subprocesses. The separate repository-limited automation token
is exposed only to the updater's publishing step, never at job scope or to
Nix. Host package change reports use job summaries without a publishing token;
this does not create isolation between trusted native-runner jobs.
Forgejo still supplies its automatic job token; `pull_request` tokens are read-only, while main/manual tokens have Forgejo's normal permissions. Do
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
Do not move staging links or sweep that directory. Use only the manual procedure
below. Manual lock-update/PR creation is implemented separately in
[nix-input-updates.md](nix-input-updates.md); it does not change retention.
Closing/merging/deleting a PR or branch does not itself remove roots. No timer or
GC policy changes.

## On-demand maintenance on Chungus

Deploy the new configuration manually **while the old runner is idle**: its
already-running process may still use the previous immediate-cancellation
settings. After deployment and provisioning the read credential below, the
normal entry point is:

```sh
sudo systemctl start chungus-nix-cache-maintenance.service
journalctl -u chungus-nix-cache-maintenance -u forgejo-runner-chungus --since today
```

There is **no timer or boot activation**. One explicit start:

1. Stops the runner from fetching new jobs and waits for its current jobs to
   finish using the native runner's graceful shutdown.
2. Runs `nix-cache-roots reconcile --apply` as `chungus-nix`, without extra groups
   or capabilities. Query or validation failures skip generation cleanup and GC.
3. Starts the **existing** `nixos-generation-cleanup.service` and waits for it.
   Its policy is unchanged: retain the newest five system generations plus the
   selected/booted generations; prune eligible system generations older than 40
   days and age-prune other conventional profiles; update boot entries; run store
   GC and optimisation. This step is intentionally privileged, not a runner job.
4. Restores the runner if it was running before maintenance, including on normal
   error paths. A previously stopped runner stays stopped. Restart failures make
   maintenance fail too.

Use this wrapper rather than directly starting `nixos-generation-cleanup` on
Chungus; the shared service alone does not drain/reconcile. Do not manually start
the runner, modify branches, run independent builds/retention commands, or start
another GC operation during maintenance. The helper's promotion lock does not
cover independent builders or prevent privileged operator interference.

The daemon receives SIGTERM **without signalling its job children**
(`KillMode=mixed`). Its native `shutdown_timeout` is 25 hours, and systemd's stop
timeout is infinite. Maintenance waits at most **24 hours** for the stop request;
it aborts before the native timeout could cancel jobs and be mistaken for a clean
drain. It also requires a successful unit result. On a stop error/timeout it runs
neither reconciliation nor GC, and queues restoration without waiting on the
unfinished stop. This means an exceptionally stuck job may eventually be canceled
at the native 25-hour limit; inspect the runner and job state before retrying.
These graceful settings apply to ordinary runner stops/restarts as well.

### Maintenance credential

Manually provision a **separate repository-read token** on Chungus at:

```text
/etc/secrets/forgejo/chungus-nix-reconcile-token  root:root 0600
```

Use the narrowest available access to `diogo/dotfiles` (prefer a dedicated
read-only identity); never the runner authentication secret, an updater write
token, or the Harmonia signing key. Token creation/provisioning is manual; no
agent reads or provisions it. The maintenance unit loads it as the systemd
credential `repository-token`. The root-owned credential is not added to the
runner service. Only the unprivileged reconciliation subprocess receives the
value as `NIX_CACHE_JOB_TOKEN`; it is not placed in argv, the Nix store or logs,
and is not forwarded to the GC service. Missing/empty credentials fail before
stopping the runner. Restart maintenance normally after a rotation to load the
new value.

The wrapper's shell trap handles ordinary failures and termination signals, not
SIGKILL or a machine crash. After abnormal termination, inspect both units and
restore the runner manually if needed. A separately started generation-cleanup
job can continue if the maintenance client is interrupted. No persistent recovery
engine or job database is added. Before the first destructive run, use the
standalone preview procedure below to inspect the planned root removals.

## Manual deleted-branch and staging cleanup

The same helper exposes only one additional command:

```sh
nix-cache-roots reconcile          # JSON preview, no root removals
nix-cache-roots reconcile --apply  # recompute and remove, never run GC
```

Both modes require the existing runner unit to be loaded, inactive/dead and have
MainPID 0. **Drain all jobs before stopping the runner**, and keep it stopped for
the entire preview/apply window. Do not run manual builds, `stage`, `promote` or
another maintenance operation concurrently. Stopping a busy runner is not a
drain: its Nix daemon build may still be finishing. The promotion lock protects
promotion, not the lifetime of a build; age thresholds would not fix that race.
The helper checks unit state but cannot prove there are no detached/manual builds
or prevent an operator from restarting it after the last check.

Run as `chungus-nix`, never root. Use either the deployed helper (locate its store
path in the runner unit's PATH) or `python3 /readable/checkout/scripts/nix-cache-roots/roots.py`.
The latter permits running a reviewed cleanup implementation without a system
deployment. Ensure Git, Nix and systemctl are on the invoking user's PATH.

1. Coordinate a maintenance window with no new jobs. Wait for running builds to
   finish, then manually stop the runner:

   ```sh
   sudo systemctl stop forgejo-runner-chungus
   ```

2. Supply an operator-held, short-lived **repository-read** token through the
   existing `NIX_CACHE_JOB_TOKEN` environment input. The Actions job token is not
   available after stopping the runner. Do not use the runner UUID authentication
   secret, an updater write token, or a signing key. No credential is installed
   or persisted by cleanup. For example, in Bash from a readable checkout:

   ```bash
   python_bin=$(command -v python3)
   read -rsp 'Repository-read token: ' NIX_CACHE_JOB_TOKEN
   printf '\n'
   export NIX_CACHE_JOB_TOKEN
   export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
   sudo -u chungus-nix --preserve-env=NIX_CACHE_JOB_TOKEN,SSL_CERT_FILE \
     "$python_bin" "$PWD/scripts/nix-cache-roots/roots.py" reconcile
   ```

3. Review the JSON actions. Existing branches retain their last successful roots
   even if their tips advanced. Only branches absent from a successful, complete
   listing of the fixed authoritative repository are removed. Every staging
   directory not selected by a remaining live branch is eligible: canceled or
   failed builds, superseded roots, metadata-only and empty allocations. There is
   no age-based preservation once the operator has established the drained window.

4. If appropriate, apply in the same stopped window:

   ```bash
   sudo -u chungus-nix --preserve-env=NIX_CACHE_JOB_TOKEN,SSL_CERT_FILE \
     "$python_bin" "$PWD/scripts/nix-cache-roots/roots.py" reconcile --apply
   ```

   Apply recomputes the plan; it does not replay a saved preview. It holds the
   shared `promotion.lock`, validates the entire inventory before deleting,
   requires two identical successful remote snapshots, rechecks unit state and
   revalidates the local plan. Authentication/network errors, empty/malformed
   listings, changed refs, corrupt selected roots/provenance or unexpected files
   abort before removals. Unexpected interrupted `.host-*` branch pointers also
   abort for operator inspection rather than guessing whether they are safe.
   Do not bypass these checks by recursively deleting directories.

5. Remove the token from the shell environment, then restart manually when ready:

   ```bash
   unset NIX_CACHE_JOB_TOKEN
   sudo systemctl start forgejo-runner-chungus
   ```

Deleted branch pointers are unlinked and their directories fsynced **before**
registered staging links are released. Only exact managed files are unlinked;
there is no recursive deletion or following of store symlink targets. Errors
after removals begin can leave partial cleanup and extra retained roots; rerun a
preview after resolving the error. No cleanup journal is needed. Nix's dangling
indirect auto-root entries are left to its normal handling. Releasing these roots
does not delete store contents, profiles, booted/current systems, or other users'
roots. A future normal GC may collect outputs no longer reachable from any root.

Remote observation and local deletion cannot be atomic. A branch recreated just
after the final query can lose its old retained output; its next successful build
will retain a fresh root. Coordinate branch changes during maintenance as well.
Standalone reconciliation never runs GC or builds/activates a system. The
on-demand maintenance unit above deliberately follows it with the existing GC
policy; neither interface adds a scheduler or timer.

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
