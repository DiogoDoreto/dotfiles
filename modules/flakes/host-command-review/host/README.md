# Host controller / authoritative ledger

Implemented with Go stdlib only, Linux-local files and flock. This is a local
trusted API, **not itself an authenticated web backend or an SSH protocol**.
`../reviewer` and the manual launcher now connect those seams; see
[operations](../docs/operations.md). Entrypoints refuse missing trusted build-time
configuration. Nothing was activated; do not advertise a live-certified reviewer.

## Integration API

* `OpenStore(absolutePrivatePath, nil)` creates/opens a private store. Do not put
  it in a project/VM share. Documented roots `/home/dog/projects` and
  `/var/lib/agent-vm` are rejected; other bind mounts/exports require deployment
  audit. Default intended path is
  `/home/dog/.local/state/agent-host-review`. Audit actual host mount/export and
  ancestor ownership at deployment; code cannot discover every VM export.
* `NewController(store, Config{Shell: fixedStoreBash})` obtains the singleton
  reviewer lock, generates a fresh session ID and epoch, recovers previous
  intents as ambiguous, revokes old approvals, sweeps orphans/retention. It never
  resumes an old session or launches on recovery. Keep the Store alive until all
  execution goroutines are reaped. `SessionID()` is routing only.
* Guest worker/integration adapter passes a validated `contracts.Request` to
  `Import`. No guest path is opened. Identical `(session,request)` submissions
  return the same snapshot; changed proposal is `ErrConflict`. Explicitly map
  local errors to coarse transport replies; do not send error strings to guest.
  `ErrFull` means the current session's outstanding-work queue is full, never
  silently drop accepted work. Defaults to 100 requests (`MaxPendingPerSession`,
  configurable 1..1024). Reviewing, approved, starting, running and output-held
  requests count; released/denied/stopped/ambiguous history and previous sessions
  do not. Duplicate lookup/conflict detection still precedes admission checks.
* Only an authenticated browser adapter may call `Heartbeat`, `Disconnect`,
  `Edit`, `Approve`, `Reject`, `Release`, or read `List`, `View`, `Held`. These
  methods do not authenticate HTTP callers. Heartbeat defaults to a two-second
  lease; browser adapter should renew every 500ms, with a deliberate disconnect
  callback. Missing/expired lease blocks approval, consumption and release.
  Lease lapse revokes approvals even if a later heartbeat reconnects.
* `Approve(id, revision, digest)` returns a HOST-ONLY binding. Never serialize it
  into guest data. TTL defaults to 30 seconds (maximum configurable 60 seconds).
  `Consume(binding)` performs ALL authority/concurrency checks and spends the
  nonce as part of the single durable `starting` record. A failure never returns
  an executable spec. Starting includes root authentication and reserves the
  global single-command slot. Edits increment revision and revoke approval.
* Preferred integration: `Run(ctx, binding, execution.Config)` consumes, executes,
  and reconciles synchronously. Run in a controlled goroutine if desired. **Never
  automatically retry any error.** The alternative `Consume`/`Reconcile` APIs
  exist for a trusted custom runner adapter, not UI/guest event injection.
* `Cancel(session, request)` serializes with consumption. Its `stop=true` means
  stop REQUESTED, not stopped. With `Run`, it also cancels the matching context;
  a custom runner adapter must send its own stop. A cancellation racing normal
  completion may return normal exit; lost/missing framed status is ambiguous.
* `Reconcile` accepts only trusted runner events. Auth cancellation returns to
  reviewing, preserving spent binding; approval must be fresh. At most 64 spent
  attempts per request. Uncertain transport/stop/submission becomes terminal
  ambiguous. Do not infer execution status from a guest message or a reusable PID.
* `Held` returns original private execution artifact. `Release` checks a fully
  reviewed artifact against original; only script/cwd/stdout/stderr may change,
  with explicit markers. Immutable release is published before record reference.
  `StatusBytes` is the ONLY guest output path: coarse status before release,
  identical artifact bytes thereafter. Polling never releases or extends age.
* `Invalidate()` is the required suspend hook: call before suspend and before
  admitting requests after resume. It revokes approvals and browser lease.
  It also cancels managed executions if the suspend-inclusive session cap has
  elapsed. Integration must fail closed until its suspend/liveness monitor is wired.
  Default clock compares suspend-inclusive `/proc/uptime` with Go monotonic/wall
  time; discrepancies >1s also invalidate. This is defense in depth, not a claim
  to detect every sub-second suspend. Four-hour session cap uses both clocks.
* `BeginEnd()` permanently blocks new work, revokes approval and cancels managed
  runs without deleting raw capture or releasing the singleton. The application
  joins runs and calls BeginEnd again (to settle auth-cancelled reviewing records),
  publishes final permitted state and closes the guest route, then calls `End()`.
  `End()` repeats BeginEnd defensively and applies clean raw cleanup. It returns an error until all starting/running records have been
  reconciled: await running goroutines, then call again. Successful clean end
  deletes raw artifacts immediately, retains releases, releases reviewer lock.
  `CloseUnclean()` is fatal shutdown: cancels managed contexts, releases singleton
  lock even on a poisoned Store; next controller recovers ambiguously.

## Ephemeral host-private live capture

Production `RunConsumed` uses `execution.ExecuteObserved`.
`RunConsumedWithObservedExecutor` is the trusted in-process observed test seam;
`RunConsumedWithExecutor` remains a compatibility wrapper for terminal-only
executors. `Reviews` adds an optional `Live *LiveCapture` for starting/running
records. Live data has a fresh random run ID (including fresh attempts after auth
cancellation), an increasing sequence, retained stdout/stderr prefixes and a
monotonic truncation flag. It is not proof of a successful outcome.

The controller validates delta offsets, 32 KiB combined delta bounds, the 4 MiB
combined retained cap, non-null streams and truncation monotonicity. Invalid
updates discard that live entry; they never create a new entry. Review reads
return defensive copies. Old callbacks cannot modify later runs or resurrect
an entry after completion, uncertainty, cancellation or shutdown. Terminal
reconciliation and every executor return clear live data; end/unclean shutdown
cancel-and-clear managed runs even if the store is unusable. Context expiry also
clears live capture. The callback takes only a dedicated live mutex: never the
store or application lifecycle locks. Store operations may take the live mutex,
never the inverse. Live output is never persisted or included in `StatusBytes`;
only explicit artifact release discloses output to the guest. No durable running
transition is required to display progress. Browser rendering and actual desktop
live-output acceptance still require the integration/live gates.

## Private layout / durability

`store.lock` and `reviewer.lock` are stable inodes; never remove/rename them.
`snapshots/ID.json`, `records/ID.json`, `raw/ID.json`, `releases/ID.json` use only
host random IDs. Raw artifacts are separate from release publication. Routing
session state/capabilities are memory-only and never revived on restart; no
browser credential is stored in the ledger. Snapshot includes original guest
request/session/proposal and host ID/import time. Editable spec/digest are separate.

Ancestors are walked using `openat` with O_NOFOLLOW directory FDs, requiring
root/current-UID ownership and no group/world write. Private final directories
are current-UID owned 0700, files 0600 with no unexpected hardlinks. This refuses
`/tmp` even for tests. No network filesystem support. Cooperating readers/writers
share exclusive flock plus per-handle mutex; flock is advisory, not a sandbox
against dog/root. Dog/root already possess arbitrary command authority.

Whole-record commit: fresh same-directory 0600 file, write, fsync file, close,
rename, fsync directory. Immutable publication: write/fsync/close, no-clobber hard
link via directory-FD anchored `/proc/self/fd`, fsync directory, unlink temporary,
fsync again. Immutable publication precedes authoritative reference; orphans are
safe and reclaimed at recovery/sweep. There is no multi-file transaction.

Any uncertain write poisons the handle: stop using it and reopen for recovery.
A dir-fsync failure after rename may leave spent starting intent; recovery marks
it ambiguous, **never retries**. Private JSON uses our exact canonical encoding
and validates semantic invariants and immutable snapshot reference. Corrupt or
incompatible records block execution globally; repair offline, never skip them.

Admission and retained history have separate limits. Defaults: 100 outstanding
requests in the current session; each file at most 8 MiB; request/spec at contract
limits. There is no global 256-record admission cap. The private history store
has a separate **2 GiB aggregate encoded-file/name byte budget** across records,
snapshots, raw captures and releases. Before writing, the store reserves the full
new file while any previous version still exists, including atomic-replacement
peak bytes. Budget exhaustion returns `ErrStorageFull` before writing and does
not poison a healthy handle or silently evict retained data. Real filesystem
metadata/locks also need disk space; this is not a filesystem quota or a promise
that the disk cannot fill. Increasing retained history increases scan cost.

Directory enumeration reads batches and has a separate **16 MiB name-byte budget
per directory**, rather than the old 4096-entry ceiling. Oversized/pathological
metadata and unexpected entries fail closed. Recovery removes unreferenced
immutable/temporary files. Retention cleanup and reads remain available when the
write-byte budget is exhausted; recovery that needs to persist state may still
fail if there is no room for its write. No endless guest polling can create extra
artifacts. Unexpected disk exhaustion/uncertain write errors fail closed.

Call `Sweep(false)` periodically; polling does not run it implicitly. Raw is
removed within 24h of import after crash, ledger/release/snapshot after seven days
of import (shorter retention than measuring from completion). Active intents are
not GC'd. Backward wall clock expires private data conservatively. Clean end
removes raw immediately, not published releases. Orphan publication has no guest
allowlist entry. Filesystem fsync semantics still need validation on lapdog.

## Validation and outstanding integration

`go test -race ./host` covers concurrent cancel/consume, one-command admission,
nonce spending, denial/no replay, dedupe/conflict, revision changes, browser
leases, explicit/clock suspend, expiry, recovery, private paths, corruption,
release/redaction, clean/age cleanup and injection before every write/fsync/close/
rename/link/temp-unlink boundary. No privileged commands or units are activated.

`../reviewer` now implements guest SSH exchange/lease advertisement and the web
Backend adapter. `cmd/agent-host-review` owns manual startup, sleep monitoring,
periodic retention and joined shutdown; lapdog also schedules cleanup-only idle GC.
Actual host path/export/fsync audit, sleep signal delivery and desktop execution
remain live gates. See `../docs/operations.md` and `../execution/README.md`.
