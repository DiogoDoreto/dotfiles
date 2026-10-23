# Host build/root helper (stage 2, task 1)

`scripts/nix-host-updater/updater.py` is a Python standard-library CLI/library.
It checks/builds **one host per invocation** and retains that branch's latest
successful system output. It does not update inputs, push commits, open PRs,
activate systems, discover/schedule branches, reconcile deletion, or run GC.
Packaging, service accounts, runner registration, and workflows are later tasks;
this helper is **not deployed** by the current NixOS configuration.

## Interface

Run manually as the eventual dedicated unprivileged build identity, not root.
Do not run these real-host build examples as an agent permission workaround.
Python 3.10+, Git, and Nix with `nix-command flakes` enabled are required.

```sh
python3 scripts/nix-host-updater/updater.py --state /var/lib/nix-host-updater status
python3 scripts/nix-host-updater/updater.py --state /var/lib/nix-host-updater \
  build-branch main --host mini
python3 scripts/nix-host-updater/updater.py --state /var/lib/nix-host-updater \
  build-branch feature/example --host lapdog --sha FULL_40_CHARACTER_HEAD_SHA \
  --action-run 'run-id/job-id'
python3 scripts/nix-host-updater/updater.py --state /var/lib/nix-host-updater recover
```

State must be persistent, private (0700), single-owner, and on a **local**
filesystem supporting `flock`, atomic rename, and fsync. Never move it while
roots are retained: Nix indirect registrations refer to absolute paths. Relative
`--state` paths are resolved once; branch names never become directory names.
Use `--` before a positional branch beginning with `-` (put options before `--`).

For Actions, pass the exact source-branch head through `--sha`, never a synthetic
merge SHA. Omit it only for manual capture of the current authoritative head.
`--action-run` is opaque metadata, not a path or an execution selector.
JSON output includes exact SHA/tree, phase results, system output/derivation,
run ID, and private log/closure-report paths. Exit codes: 0 successful operation,
2 unsuccessful/superseded host build or pending publication recovery, 1 invocation
or state error. Each host job invokes the helper independently; a failure has no
effect on another host's invocation/root. Future matrix jobs must disable fail-fast.

| Host | Flake | Output under `nixosConfigurations` |
| --- | --- | --- |
| lapdog | `hosts/lapdog` | `lapdog.config.system.build.toplevel` |
| chungus | `hosts/chungus` | `chungus.config.system.build.toplevel` |
| mini | `hosts/mini` | `dogdot.config.system.build.toplevel` |

The only remote is the configured authoritative repository:
`ssh://forgejo@git.local.doreto.com.br:2222/diogo/dotfiles.git`, named `forgejo`
inside each isolated checkout. There is deliberately no remote URL override.
Any existing **trusted** branch can be explicitly built, including non-bot PR
branches; branch existence does not cause a build or a root. No six-root limit.

## Source and checks

The helper validates a full `refs/heads/...` ref, queries Forgejo, fetches that
branch with its history, and checks out the exact commit detached. Fetch drift
is a superseded outcome, not permission to build a different revision. Each run
has an independent checkout of the **whole repository**, preserving shared
imports, Git revision metadata, and host flake `self` semantics. Nix gets an
explicit `git+file` URL with `dir=hosts/<host>` and the captured `rev`.

For the selected host it runs `nix flake check`, builds the explicit toplevel,
and queries the recursive closure. Both check/build use `--no-update-lock-file`
and `--no-write-lock-file`. A committed host lock is required; all checkout lock
contents and Git cleanliness are checked after each command. Unexpected mutation
fails the run. No lock update, impurity flag, or source-metadata stripping occurs.

## Retention and concurrency

```text
STATE/manifest.json                       # atomic manifest + promotion journal
STATE/lock                                # shared state/publication flock
STATE/work/RUN/                           # disposable full checkout
STATE/runs/RUN/lock                        # held by controller throughout run
STATE/runs/RUN/output                      # Nix-registered indirect GC root
STATE/runs/RUN/commands.log, closure.json   # private diagnostics
STATE/roots/SHA256(remote)/SHA256(full-ref)/HOST  # atomic branch pointer
```

Nix's `build --out-link STATE/runs/RUN/output` supplies process temporary roots
while realizing the output and registers a persistent indirect root before
exiting. The output link is outside the checkout and remains registered during
promotion and recovery. The branch pointer links to this registered link, not
to an unregistered staging path. The winning run's output link becomes that
branch/host's retained root. Keep both the pointer and its selected run directory.

Builds/checks do not hold the shared lock. Promotion holds it while checking the
authoritative remote head, journaling old/new metadata, atomically replacing the
branch pointer, and committing the success record. Monotonic start sequence
numbers prevent an older completion from displacing a newer successful run,
even after force-pushing back to the older SHA. Hosts have independent slots.
The prior registered link is unlinked only after the new metadata is durable;
cleanup intent remains journaled until complete. Only managed output links for
the replaced slot (or a definitively failed unpublished run) are unlinked.
Unrelated roots are untouched; no production GC is invoked.

An authoritative query failure, changed head, or missing branch prevents
promotion and preserves the previous success. A failed refresh does not make
the old SHA pass for the new revision. There is no atomic transaction spanning
Forgejo refs and local filesystem roots: the remote can move immediately after
the last head check. Metadata records **the observed head and check time**, not
a distributed freshness guarantee. Future manual reconciliation must recheck.

## Status and recovery

`status` is read-only apart from creating missing empty state directories/lock
files. It does **not** query Forgejo: `stale: false` means consistent with the
last local observation and latest local attempt, not proof of current remote
freshness. It reports latest-attempt failures/pending work, changed/deleted known
heads, inconsistent/missing roots, interrupted controllers, and pending journals.
Run liveness uses a per-run OS lock, not PID existence or timestamps.

`recover` (also run before a build) takes the shared lock and:

- If the branch pointer already selects the journal's new registered link,
  and that link is itself a symlink directly targeting the recorded existing
  system output, finishes that historical promotion's metadata/old-link cleanup
  idempotently. The full chain is checked again before releasing the old link.
- If it still selects the old link (or was absent for a first build), records
  interruption and does **not** publish the new output without a fresh build/head
  check. The uncertain new registered link is preserved.
- If the pointer, output link, or recorded target disagree otherwise, refuses to
  guess and preserves both old and new links plus the pending journal. Resolving
  to just any existing output/directory is not sufficient. `status` uses the same
  full-chain validation and marks a mismatched root inconsistent/stale.
- Marks abandoned active controllers interrupted. It does not restart checks,
  build anything, query the remote, or infer branch deletion.

On cancellation a child Git/Nix process may outlive its controller. An interrupted
controller is not proof every child exited. Retain uncertain output links and
inspect private logs/processes before any later cleanup. Interrupted links,
checkouts, logs, and run history are deliberately not age-pruned here. In
particular, old run metadata alone is **not** evidence its output can be removed.
After recovery, explicitly retry `build-branch` if needed. Corrupt manifests or
unexpected pointer targets require operator investigation; do not delete state
or roots to silence an error.

Later reconciliation must use this same shared lock, repair pending journals,
check per-run liveness, and preserve uncertain/in-flight registered links. No
reconciliation/deletion interface or retention timer is implemented in task 1.

## Trust and secrets

Only repository branches whose authors are trusted to populate the signed cache
may be passed to the helper. The fixed remote is not itself an approval policy:
fork/untrusted PR exclusion must be enforced by the later repository-scoped
runner/approval design, not merely editable workflow YAML. The helper does not
execute repository-supplied scripts, but trusted Nix expressions can build code;
this Python controller is not a sandbox against malicious repository authors.

Commands use argument arrays, disable Git hooks/global Git config and non-SSH
transports, and require strict verified SSH host keys. Provision read-only fetch
access/known-hosts separately. The child environment is allowlisted: inherited
tokens, Git injection variables, `NIX_CONFIG`, agent sockets, and signing settings
are not forwarded. Helper-created temporary files live under persistent run
directories. Nix daemon build-directory/sandbox policy remains host configuration.

No signing-key path, token, push credential, or signing operation is accepted or
needed. **Actual secret inaccessibility requires the task-2 Unix/service account
permissions**: run with a private HOME, no updater write credentials, no activation
privileges, no Nix trusted-user status, and no read access to Harmonia's secret.
Do not run this helper as the cache service, root, or a user owning signing keys.
No account/permission boundary has been deployed or tested by this task.

## Development validation

```sh
python3 -m unittest discover -s scripts/nix-host-updater/tests -v
NIX_HOST_UPDATER_REAL_NIX=1 python3 -m unittest discover \
  -s scripts/nix-host-updater/tests -v
git diff --check
```

Default tests use real temporary bare Git repositories and a fake Nix subprocess.
They cover host independence, branch/path safety, full-layout exact commits,
lock mutation, failures/refreshes, remote loss/drift/deletion, concurrent stale
completions, cancellation, and crashes around pointer/metadata/cleanup changes.
Regression cases also corrupt a replacement's output link after pointer
publication (another existing store output, an unrelated directory, or a
non-symlink); recovery must preserve the prior guard and refuse cleanup.
The opt-in test uses tiny `/bin/sh` derivations and a **custom local store URL**
with store/state/log/build-temporary directories under `.private/tmp/`. It
disables sandbox/substituters/build-users **only in that disposable test store**;
it never builds a real host or runs GC against the production daemon/store.
Actual GC proves the registered links retain old/in-flight outputs, replacement
releases obsolete successful roots, failed refresh preserves success, and
checkout deletion does not remove retention. Test temporary directories are
cleaned after each case; no isolated root is left registered in production.

Remaining rollout gates: package/install/accounts and credential isolation,
dedicated runner/workflow pilots, manual reconciliation/uncertain-root cleanup,
real host closure builds, and end-to-end cache/Actions acceptance. These tests
establish local build/retention mechanics, not runtime or activation safety.
