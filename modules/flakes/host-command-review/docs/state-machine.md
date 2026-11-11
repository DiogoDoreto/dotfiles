# Host authority, state machine and recovery

This is the normative unreleased v1 policy. `Next` checks only table edges;
`host` and `reviewer` implement authority and orchestration. Live desktop gates
remain pending (see operations.md and probe/FINDINGS.md).

## Session lifetime

1. `dog` manually starts `agent-host-review` on lapdog in the local Plasma session.
   Create a fresh random routing ID, epoch and browser auth capability; obtain a
   single-reviewer lock. Session hard maximum: four hours, measured by a monotonic
   clock. Wall-clock approval expiry is an additional check, not a substitute.
2. Advertise active reviewer routing to the guest only while the host is alive.
   With no active reviewer, submissions get `rejected/no_reviewer`, never queued
   for an eventual future reviewer. Guest stale advertisements cannot prove host
   liveness: use a host-renewed, short, expiring guest lease; fail closed on expiry
   or loss of SSH and reject pending unacknowledged submissions. The guest adapter
   may report provisional acceptance only as `pending`, not host acceptance. A
   malicious guest can fake any guest status but cannot thereby gain host authority.
   Lease duration/renewal and queue bounds must be explicit worker policy and tested.
3. Browser disconnect immediately blocks new approvals and nonce consumption.
   Already-consumed approved work may finish. Do not infer disconnect solely from
   HTTP TCP keepalive: require an authenticated browser liveness lease/heartbeat.
   Reconnect requires valid browser authentication; old approval UI cannot bypass
   revision, nonce, epoch or expiry checks.
4. Suspend/resume invalidates all unconsumed approvals, rotates epoch and requires
   renewal. Rotate/deny before suspend when possible; after resume, fail closed
   before allowing consumption. Recheck session expiry on resume (monotonic clocks
   can exclude suspend). Consumed work is never made executable again; if a live
   supervisor's outcome cannot be recovered, use `ambiguous`.
5. Reviewer death/session end cancels the process tree, including a root command.
   This must be enforced by the supervisor/service lifetime independently of a
   cooperative host process. Stop grace five seconds, then kill the entire cgroup.
   Session renewal does not reconnect to a dead session's approvals.

## Request phases

A host-pulled, validated, durably snapshotted proposal starts `reviewing` at
revision 1. Reject malformed data before creating a record; no arbitrary guest
path is ever interpreted as a host store path.

| From | Event | To | Additional host guard/effect |
| --- | --- | --- | --- |
| reviewing | edit | reviewing | increment revision; rebuild frozen spec/digest |
| reviewing | invalidate | reviewing | new epoch; discard any stale UI capability |
| reviewing | approve | approved | authenticated live browser/reviewer; complete spec, current revision/digest, fresh random nonce and expiry |
| reviewing | reject | denied | host reviewer decision; no execution |
| reviewing/approved | cancel_before_start | stopped | serialize against nonce consumption; cannot mean stopped after consumption |
| approved | edit/invalidate | reviewing | increment revision on edit; revoke nonce; renewal required |
| approved | consume | starting | atomically validate and spend nonce; commit execution intent **before** prompt/launch |
| starting | auth_denied | reviewing | known pkexec denial/no launch; old nonce stays spent; fresh approval needed |
| starting | started | running | positively identify supervisor/cgroup; do not trust guest claims |
| starting | finish | output_held | known pre-exec failure with captured host outcome; never assume an uncertain start failed |
| starting/running | lose_execution | ambiguous | cannot prove whether/how execution occurred; never rerun automatically |
| running | finish | output_held | process tree reaped/stopped; durable outcome and capture metadata |
| output_held | release | output_released | authenticated explicit disclosure of reviewed immutable artifact |

`output_released`, `denied`, `stopped`, `ambiguous` are terminal in v1. No retry
edge. To attempt again, make a new request and fresh review, prominently showing
any ambiguous prior execution. Do not offer a hidden automatic retry button.

Cancellation after consumption is a supervisor stop request, **not** the
`cancel_before_start` edge. Record `cancelled` exit kind after the tree is actually
stopped, then hold output. A cancellation racing with normal completion may
produce a normal exit. Don't claim cancellation reversed side effects. Unknown
outcomes are `ambiguous`; unknown raw output must never be implicitly released.

Guest coarse view: reviewing/approved -> pending; starting/running -> active;
output_held -> held; output_released -> released; denied -> rejected; stopped ->
cancelled; ambiguous -> uncertain. Before release no exit status, edits, shell,
root authentication outcome, execution timestamps or output. Phase transition
timing is not a secrecy guarantee; only the fields are deliberately restricted.

## Atomic approval guards

Under the host ledger lock verify all of: active session, live reviewer, browser
lease, unexpired approval, same epoch, matching snapshot/revision/digest, unspent
nonce, phase approved, and no other command in starting/running. Concurrency is
one command **including the root authentication interval**. Recompute the digest
from the stored frozen spec; never hash a guest-owned file or a stale browser form.
Durably replace the record with phase `starting` and spent nonce before starting
pkexec or any process. Failure to persist means do not start. Failure after persist
is potentially ambiguous even when it occurred before `running` was recorded.

Root must require a **per-command** KDE prompt using `pkexec` and a dedicated
polkit action with `auth_admin` and no `_keep`. The short-lived root supervisor /
transient systemd execution is not a persistent listener/helper and exposes no
VM-accessible host control surface. It must validate host-approved immutable input
and enforce reviewer lifetime/timeout/cgroup cleanup. Details require the separate
root-execution proof; do not replace these with `sudo`, a persistent root daemon,
a cached grant, or guest-supplied pkexec arguments.

## Frozen execution and output

Freeze script bytes, cwd string, user dog/root, resolved fixed bash path, clean
closed environment, timeout (default 120 seconds, maximum 1800), stop grace (5),
and combined capture limit (4 MiB). Execute bash with
`--noprofile --norc /proc/self/fd/3`, immutable unlinked script file on FD3,
NEVER script text in argv/environment, stdin `/dev/null`, no PTY, no startup
files and no inherited environment. This amends unreleased v1's earlier `-c`
wording; `$0` and `BASH_SOURCE` identify `/proc/self/fd/3`. Digest struct encoding
is unchanged: the fixed invocation is version policy, not a guest field. The fixed
PATH and cwd still reference **live mutable filesystem content**. A script can
read/source live files and itself start interactive or networked software; the
runner's lack of terminal does not make arbitrary scripts safe. Snapshots freeze
command text, **not filesystem semantics**. Reviewer accepts arbitrary host command
authority and secret exposure, not a sandbox guarantee.

Capture stdout+stderr combined up to four MiB in aggregate; keep draining both
pipes concurrently after the limit, discard excess and mark `truncated=true`.
Never deadlock on a full pipe or grow memory/disk without bounds. Ordering across
the two streams is unspecified. A timeout includes command execution; root prompt
and overall starting lifetime must also be bounded by an explicit supervisor
policy/session expiry (never an unlimited prompt). Kill descendants, not just bash.

All output and execution metadata are held host-private by default. The reviewer
may release an immutable artifact containing reviewed executed spec, digest,
stdout/stderr, exit info, truncation and explicit redaction markers. Execution
metadata can itself contain secrets. Releasing output must show metadata too;
never label the original guest proposal as the executed command. A redaction is
explicit and reviewed; it is not a silent substitution of executed history. Retain
an original host record/digest until cleanup; publish a separate release artifact.
Once published, identical retries return the identical artifact, not regenerated
JSON with different IDs or current edits.

## Crash and cleanup

* No execution side effects until the durable starting/nonce-consumed record.
* On recovery, reviewing/approved records belong to a dead session: invalidate
  approvals; never resume automatic launch. A starting/running record without
  positively recoverable supervisor/outcome becomes ambiguous. Never infer that
  a missing PID means the script never ran; PIDs are reusable.
* Supervisor ownership/lifetime needs boot/session identity and an independently
  enforced watchdog. A ledger record alone cannot stop an orphaned root process.
* Clean end: cancel/reap work, delete raw capture immediately, revoke session and
  browser capabilities. Do not delete a published release with the raw directory.
* Crash leftovers: host-private raw data expires within 24 hours. Released
  artifacts and ledger records expire after seven days. Preserve raw references
  only while needed, never extend retention by guest polling. Sweep with the same
  lock and validated host-only paths. Age checks must tolerate clock changes;
  fail closed on corrupt metadata without executing, isolate for safe cleanup.
* Retention and durable tombstones mean idempotency is bounded by seven days and
  the originating session lifetime. Expired session IDs are never reactivated.
