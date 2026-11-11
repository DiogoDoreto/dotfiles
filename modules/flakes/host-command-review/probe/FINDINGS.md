# Bounded command execution feasibility proof

## Verdict and scope

**Proceed to a production transport implementation, not deployment.** Go's standard
library suffices for the framing, exact-byte approval binding, process invocation,
private pipes, capture budget, and supervisor state machine. Source supports using
`systemd-run --pipe --wait` for transient-service FD passing and waiting, plus a
bounded `systemctl stop` subprocess for cancellation. No D-Bus Go dependency is
needed for that design. **This proof does not certify the actual systemd/polkit
transport.** The live acceptance tests below are release gates.

The naive version is insufficient: do **not** put `sh -c SCRIPT` in the transient
unit, connect its stdin directly to reviewer control, trust a raw exit code as the
runner protocol, or assume killing `systemd-run` kills the unit. A small immutable
in-unit execution helper is required to receive the manifest privately, enforce a
second start gate, separate script/control/stdin, and frame payload results. That
helper and the actual systemd transport are deliberately **not implemented** here.
No speculative root implementation is being presented as secure.

This independent module is a disposable proof, not the main package or its
manifest contract. No Nix files, host configurations, mini services, activation,
polkit authorization, root commands, or transient units were changed/run. All
process tests execute unprivileged children of `go test` in the agent environment.
The installed CLI reports systemd 262; this says nothing about lapdog's running
version. Upstream `latest`/`main` source links below are moving references, not
assertions that lapdog runs that revision.

## What is implemented and tested

Run from this directory:

```sh
go test -race -count=3 ./...
go vet ./...
# If Go is not on PATH, from the repository root:
nix shell nixpkgs#go --command sh -c \
  'cd modules/flakes/host-command-review/probe && go test -race ./... && go vet ./...'
```

Verified using Go 1.26.10; `go.mod` declares Go 1.23 and imports only stdlib.

| File | Proven, within the stated test assumptions |
| --- | --- |
| `protocol.go` | Bounded 64-KiB frames over byte streams; partial reads/coalescing, truncated frame/length rejection; exact-byte SHA-256 binding, including whitespace differences; canonical lowercase digest syntax |
| `supervisor.go` | Manifest → fresh random READY challenge → matching START; no launch on EOF/cancel/replay/partial frame/timeout before START; lease renewal and expiry, cancellation/EOF/runtime stop, async submission cancellation, explicit unconfirmed-stop outcomes |
| `process_linux.go` | Actual Linux subprocess, `/dev/null` stdin, explicit environment/cwd, anonymous unlinked script file inherited as fd 3; separate stdout/stderr pipes, combined 4-MiB retention then continued drain; bounded drain after main exit; unprivileged process-group cleanup |
| `plan.go` | Validated random-looking unit-name syntax, store executable syntax, fixed CLI properties, no manifest/script/cwd/env in generated unit argv; illustrative pkexec outcome classification |
| `*_test.go` | Mocks for start/stop failure and success racing cancellation; real child runner using only stdin/stdout control, real process cancellation, background child retaining output, 10 MiB output without deadlock, output pipe deadline |

Earlier stress run: `go test -race -count=10` passed before the real stdio transport
tests were added. Final verification: `go test -race -count=3 ./...`,
`go test -cover ./...` (84.3% statement coverage), and `go vet ./...` all passed.
Timers in tests are intentionally short; production durations must not be copied
from tests.

### Explicit non-proofs / integration obligations

* `VerifyApproved` is **not a manifest validator**. Production must validate the
  immutable exact manifest, script, cwd, identity, timeout, clean environment and
  shell using the parent's contracts before any backend is called. It must not
  validate one serialization and execute a different reconstruction.
* `Backend.Start` must honor its context; on error it must leave no unreported
  execution, and on a creation/cancellation race it must return the created handle
  for cleanup. The state machine tests that contract with a cancellation-aware
  mock. The actual systemd implementation still needs to prove this, especially
  when D-Bus disconnects during `StartTransientUnit`. If submission cannot be
  confirmed, report uncertainty, never successful cancellation.
* The callback used for READY must itself be deadline-bounded. Actual Go Linux
  pipes support `SetWriteDeadline` (tested). A general arbitrary `io.Writer` does
  not promise that. Payload-output delivery cannot run synchronously in the
  lifecycle loop; use bounded queues/deadlines and keep draining unit pipes after
  retention/client-delivery limits. Input closure must unblock Read; tested with
  real OS pipes and `io.Pipe`, not arbitrary custom readers.
* `LocalProcess` is **only** a process-group test utility. It has no independent
  runtime timer after its parent dies, and `setsid` can escape it. It is never the
  proposed root backend. It kills ordinary descendants immediately on normal main
  exit for test cleanup; it is not the production graceful-stop policy.
* The script fd experiment uses an unlinked mode-0600 file in a private temp
  directory. Production should create it in a root-owned private runtime directory,
  unlink immediately, and only inherit the intended fd. A pipe alternative needs a
  concurrent writer: pre-filling a script-sized pipe can deadlock. Validate shell
  compatibility with `/proc/self/fd/3` on lapdog. Shell expansion/filesystem contents
  can change after approval: approval of command bytes is not a filesystem snapshot.
* Unit/helper output framing and terminal result emission are not a production
  implementation here. The real stdio subprocess test uses a test-only terminal
  JSON frame. Its constants and wire shape are not proposed main-package contracts.
* SHA-256 binds reviewed bytes; it is not a signature, proof of a human click, or
  secret. Any process running as `dog` can submit its own digest and ask for auth.
  We are not defending against a malicious reviewer running as the same identity.

## Recommended lifecycle and transport

### Reviewer → pkexec → one-command root supervisor

Reviewer is manually started by `dog` in lapdog's local active Plasma session.
Spawn directly (no shell):

```text
/run/wrappers/bin/pkexec --disable-internal-agent \
  /nix/store/HASH-host-command-review/bin/host-command-root-runner \
  --approved-sha256 LOWERCASE_DIGEST
```

Pin all executable paths at Nix build time, including `systemd-run`, `systemctl`,
helper and shell. No PATH lookup in the root implementation. Reviewer creates
private stdin/stdout/stderr pipes. `pkexec`'s source sets CLOEXEC on every fd >= 3
before executing the target: pre-created extra control sockets/FDs will **not**
reach it [P1, P2]. Packet IPC is not required: reliable stream pipes with a bounded
length prefix suffice. Use one writer per framed output stream, write-full loops,
maximum sizes before allocation, strict state transitions, and frame deadlines.
Do not rely on line boundaries or JSON delimiters for arbitrary script/output.

Do not fill stdin with START before authentication. Manifest can be sent in a
bounded write goroutine; after auth the runner validates its bytes/digest/schema,
then sends a cryptographically fresh READY challenge. The live reviewer responds
with the matching START, then empty LEASE frames (e.g. 500 ms interval, 2 s expiry)
or CANCEL. A complete handshake should have a finite post-auth deadline (e.g. 5 s).
Partial frames do not extend leases. An idle/malicious stream cannot hold a root
runner indefinitely. Before authentication the unprivileged reviewer must also
have an auth/cancel timeout and close/kill/reap its pkexec child when abandoned;
that does not substitute for the post-auth handshake.

The response establishes peer participation **after auth**; it cannot make
"reviewer still alive at the exact next instruction" race-free. Reviewer death
immediately after START can race creation. Anonymous pipe EOF is the primary
death signal. Do not use `kill(pid, 0)` or a PID alone: PID reuse and inherited
writer FDs defeat that claim. The reviewer must not leak its control write end to
children; Go's normal `os/exec` closes unrelated fds. Duplicating the write end can
suppress EOF, so a finite lease is also required. If separate process identity
checks are desired, use a pidfd or PID + `/proc` start time, never as a replacement
for the control channel. No hand-written pidfd/DBus protocol is necessary here.

### Root supervisor → transient service

`UnitPlan` generates (but does not execute) this shape:

```text
systemd-run --system --no-ask-password --quiet --wait --pipe --collect \
  --service-type=exec --expand-environment=no \
  --unit=host-review-RANDOM_128_BIT_HEX.service \
  --property=RuntimeMaxSec=120.000 \
  --property=TimeoutStartSec=5s --property=TimeoutStopSec=5s \
  --property=KillMode=control-group --property=SendSIGKILL=yes \
  --property=Restart=no --property=UMask=0077 \
  IMMUTABLE_HELPER --unit-payload --approved-sha256 DIGEST
```

Use random unique names, fail on collision, never replace an existing unit. Defaults:
2 min command runtime, explicit maximum 30 min, 5 s termination grace. This plan
rejects runtimes below 1 ms so formatting cannot silently round them to zero.
Production contract may require a coarser minimum. Do not use `Type=oneshot`:
`RuntimeMaxSec` does not apply to it [S2]. Do not add restart/remain-after-exit or
payload-controlled timeout extensions. Keep the helper's pre-start wait inside the
same finite unit runtime and give it a separate short handshake deadline.

`--pipe` passes **the caller's stdin/stdout/stderr descriptors as-is**, via the
manager's `Standard{Input,Output,Error}FileDescriptor` properties [S1, S4]. Connect
these to **new private pipes**, not the outer reviewer's control fd. This is
systemd's existing D-Bus Unix-FD transport; Go need not implement it. `--wait`
waits for unit deactivation; `--quiet` suppresses normal runtime summaries, not
all diagnostics. `--collect` unloads failed units too [S1].

The immutable helper must:

1. Receive bounded manifest bytes on its private stdin, verify digest/schema again,
   and report UNIT_READY before payload launch. It cannot assume `PKEXEC_UID` is
   present in the manager-created service. Outer runner verifies its root identity
   and pkexec caller; unit helper validates target root and trusted manifest policy.
2. Require a fresh unit-start response from the outer supervisor **after** UNIT_READY,
   with a short deadline. The outer supervisor sends it only while the reviewer
   lease/control is still valid. This closes the practical delayed-creation hole:
   if the supervisor dies during submission, a unit created later cannot simply
   launch an already-buffered script. Do not pre-buffer the second START with the
   manifest. A death just after the second START still needs RuntimeMaxSec fallback.
3. Prepare the script fd, set validated cwd, set exact clean environment (do not
   inherit system manager environment), enforce root UID/GID and intended groups,
   then start the immutable shell with `/proc/self/fd/3` and stdin `/dev/null`.
   Neither `sh -c SCRIPT`, script text in environment, nor `WorkingDirectory=` /
   `Environment=` containing manifest values in unit metadata is appropriate.
4. Capture payload stdout/stderr on its own pipes and frame them, plus a terminal
   payload status, on its private output. Only helper-generated framing reaches
   outer status parsing: raw payload output must never masquerade as control.
   Root payload is not considered malicious in this trust model.

Thus there are two **short-lived, per-command** trusted roles: outer supervisor
and in-unit helper (plus payload). No reusable privileged listener/daemon. The
in-unit helper can be a separate immutable binary or a carefully validated private
mode; do not expose a CLI mode that skips manifest/identity/start-gate checks.

`systemd-run` stderr is transport diagnostics, not payload stderr under this helper
arrangement. Its stdout is the helper's framed stream. Never forward raw transport
diagnostics into the runner-control parser. Capture streams privately with a
combined 4-MiB retention budget, separate byte counts/truncation indication, then
drain/discard until completion. Set bounded output writes to a disappearing
reviewer. Retained output is also untrusted terminal text and may contain secrets.
No script/cwd/env/payload-output goes in CLI arguments, descriptions, unit
properties or normal journal logging. Unit metadata will reveal runner/helper path,
digest, unit ID, runtime, and exit result. This is not a guarantee against a payload
explicitly logging to journald, kernel/audit/coredump facilities, or malicious root.

### Stop, death, timeout and uncertainty

On CANCEL, EOF, expired lease, invalid frame, or runtime/context cancellation,
stop the named unit using an absolute-path, sanitized-environment child:

```text
systemctl --system --no-ask-password stop host-review-ID.service
```

Bound that CLI's wait; don't wait forever for a bus/job. The service's own
`TimeoutStopSec=5s`, `KillMode=control-group`, `SendSIGKILL=yes` provide TERM then
KILL for processes still in its cgroup [S2, S3]. Allow small finite transport
slack beyond the 5-s grace when reporting confirmation. An unavailable/hung bus
means **stop-unconfirmed**, not success. Killing the CLI subprocess bounds the
runner but is not evidence the service stopped. Handle cancel-before-submit,
cancel-during-submit and late UNIT_READY explicitly; retry StopUnit for a pending
submission within a bounded window, and never release the helper's second gate
after cancellation. Do not reuse the unit name.

The runner must close its helper input when abandoning submission. If it is
SIGKILLed after launching the command, systemd's independent runtime limit still
ends ordinary processes remaining in the unit, followed by stop grace. The bound
is measured from service activation, not reviewer click or authentication start;
startup and handshake need their own deadlines. It is not a hard real-time bound:
kernel uninterruptible tasks/manager failure cannot be made killable by Go.
Root payload can move cgroups, start another unit, modify policy, or disable the
manager. **No malicious-root containment promise.**

`--collect` races post-exit `systemctl show`: properties may already be gone.
Use the private helper's terminal result for payload exit, not a post-collection
property query. If the helper dies before terminal status, report execution lost
or uncertain, even if the CLI exit happens to be zero. If production cannot
preserve private status/start gating with the CLI implementation, stop at that
blocker and use a small maintained D-Bus library for `StartTransientUnit`,
`StopUnit`, job/property signals, and Unix FD passing. Do not hand-roll D-Bus.

### pkexec exit status is not the payload protocol

pkexec returns the program's exit code; auth cancellation is 126 and authorization /
authentication errors are 127 [P1]. So a bare exit code is inherently ambiguous.
Require a framed RUNNER_READY after authentication and a terminal runner result:

* Before RUNNER_READY, 126 is auth-cancelled under the runner's reserved-exit-code
  convention; 127 is auth-error **or runner failed before READY**. Production runner
  should reserve a distinct internal failure status (e.g. 70) and never voluntarily
  return 126/127. Absolute proof of which executable failed before READY is not
  available from exit code alone; UI must preserve that uncertainty.
* After RUNNER_READY, absence of a terminal frame is runner/transport loss, never
  auth denial. The helper's payload result distinguishes real payload 126/127.
* After a valid terminal frame, report that framed result, not the pkexec exit
  status as though it were the command status. Malformed/missing/truncated final
  frames are not success.

`ClassifyPkexec` documents these states. It intentionally labels pre-ready 127
ambiguously. Parent production contracts must decide result precedence if the
runner emits a terminal frame and is then killed before a clean process exit.

## NixOS / polkit sketch — documentation only

NixOS's current module declares `enablePkexecWrapper` via `mkEnableOption`, so
its default is false. Its `security.wrappers.pkexec.enable` is exactly that option
[N1]. Enabling polkit alone does **not** establish this wrapper. A future lapdog
module needs both:

```nix
security.polkit.enable = true;
security.polkit.enablePkexecWrapper = true;
```

Install a dedicated policy XML from the immutable package, interpolating its
**exact store runner path**, not a mutable symlink:

```xml
<action id="br.com.doreto.host-command-review.execute">
  <description>Run one reviewed host command as root</description>
  <message>Authenticate to run one reviewed host command as root</message>
  <defaults>
    <allow_any>no</allow_any>
    <allow_inactive>no</allow_inactive>
    <allow_active>auth_admin</allow_active>
  </defaults>
  <annotate key="org.freedesktop.policykit.exec.path">IMMUTABLE_RUNNER_PATH</annotate>
  <annotate key="org.freedesktop.policykit.exec.argv1">--approved-sha256</annotate>
</action>
```

A narrow polkit rule, ahead of any broad allow rules:

```js
polkit.addRule(function(action, subject) {
  if (action.id !== "br.com.doreto.host-command-review.execute") return;
  if (subject.user !== "dog" || !subject.local || !subject.active ||
      action.lookup("user") !== "root" ||
      action.lookup("program") !== "IMMUTABLE_RUNNER_PATH")
    return polkit.Result.NO;
  return polkit.Result.AUTH_ADMIN; // NOT AUTH_ADMIN_KEEP
});
```

`exec.path` selects the dedicated action; optional `argv1` narrows it to the runner
entrypoint. pkexec does not validate remaining args: the runner must accept only
its exact intended invocation and check digest syntax. Rules must check actual
`program` and target `user` details, **not command_line**, which pkexec documents as
unsafe for security checks [P1]. `subject.local`, `subject.active`, rule ordering,
and AUTH_ADMIN versus AUTH_ADMIN_KEEP are documented in [P3]. Admin identities
follow NixOS's configured admin rule (default wheel), not necessarily `dog` alone.
If only dog's administrator credential is desired, add a similarly action-scoped
admin-identities rule rather than changing global admin policy silently.

**Ordering/action selection is an acceptance gate.** Another earlier broad rule
could short-circuit this rule; `argv1` mismatch may fall back to generic pkexec
policy instead of the dedicated action. Dedicated deny rules are not a global
restriction on dog's existing sudo/generic-pkexec authority. Inspect the effective
policy order, verify exact action selection on the installed version, and reject
unexpected invocations in the runner. Never add `allow_gui`, auth caching (`*_keep`),
a persistent privileged helper, or a wildcard path binding. Normal Plasma's
registered session authentication agent is required; `--disable-internal-agent`
prevents fallback to a textual agent [P1].

## Live lapdog desktop acceptance gates — ALL PENDING

These require the user's explicit later activation/testing; none were performed.

1. Build immutable runner/helper and lapdog policy; verify effective wrapper path,
   exact store action binding, option availability against lapdog's lock, rule
   ordering, root target, and Plasma's registered agent. No mutable executable.
2. As local active dog, get a GUI auth prompt **for every command**, including two
   adjacent identical requests; cancel → 126 classification, wrong credential /
   absent agent → error classification. Verify no internal/terminal fallback.
3. Reject inactive/local-other-user/remote session/non-root target and wrong program,
   argv1, duplicate/unknown flags, malformed/changed digest/manifest, unsafe cwd,
   non-store shell, bad identity/environment and out-of-range timeout. Verify
   actual action IDs to expose generic-action fallback rather than assuming denial.
4. Kill reviewer while auth prompt is up; approve afterward: no payload. Delay unit
   creation, cancel/kill runner while submission is pending; a late helper must
   time out/EOF without launching. Verify the second start challenge, no prebuffered
   START, stale nonce rejection and caller identity checks.
5. Exercise actual `systemd-run --pipe --wait --quiet --collect`, confirm three
   private inherited FDs, no PTY, separate command stdin `/dev/null`, script via fd 3,
   exact cwd/identity/groups/env, working shell and separate stdout/stderr framing.
6. Run success, payload 126/127, signal death, helper crash, start/exec failure,
   manager disconnect/restart and unit collection races. No exit code alone yields
   payload success. Terminal-frame loss is uncertainty, not auth rejection.
7. Fork grandchildren (including `setsid`), ignore TERM, exit main while children
   remain, fill both output pipes, retain inherited FDs and exceed 4 MiB; cancel and
   timeout remove ordinary cgroup processes within configured grace. Drain stays
   bounded; UI reports truncation, exit/signal status, timeout and stop uncertainty.
8. Kill reviewer, freeze reviewer (lease), close/partially write control, kill outer
   runner, kill systemd-run CLI, disconnect bus. Confirm prompt EOF cancellation or
   independent RuntimeMaxSec + grace for a started payload. No orphan root listener.
9. Inspect process command lines, `systemctl show`, journal and error paths: only
   fixed paths/digest/unit ID, no script/cwd/env values or captured stdout/stderr
   from normal transport. Check helper diagnostics and failure paths too.
10. Verify 2-min default, 30-min maximum, 5-s TERM/KILL grace; start/handshake/stop/
    output waits finite; no restart or inherited fd prevents cleanup. Test runner
    death just before and just after both start gates. Observe actual systemd
    startup/stop latency; do not claim hard-real-time guarantees.

## Generic unprivileged user execution

Use the same manifest validation, start gate, leases, captures, framed status and
bounded cleanup without pkexec/auth. Prefer an analogous transient **user service**
(`systemd-run --user`, `systemctl --user`) to retain cgroup cleanup and independent
RuntimeMaxSec after reviewer death; verify the user manager can receive FDs and
provide the intended environment. A direct Go process-group backend is useful for
tests but cannot meet independent runner-death/setsid cleanup guarantees. Do not
quietly downgrade to it in production if the user manager is unavailable.

## Primary sources reviewed

* **P1:** [pkexec manual](https://www.freedesktop.org/software/polkit/docs/latest/pkexec.1.html),
  RETURN VALUE, AUTHENTICATION AGENT, SECURITY NOTES, ACTION AND AUTHORIZATIONS,
  VARIABLES: codes 126/127, internal-agent switch, path/argv1 binding, minimal
  environment, no argument validation, unsafe command_line.
* **P2:** [pkexec.c](https://github.com/polkit-org/polkit/blob/master/src/programs/pkexec.c),
  `fdwalk(set_close_on_exec, GINT_TO_POINTER(3))`, saved `PKEXEC_UID`, target group
  and UID setup. This directly rules out relying on inherited fd >= 3 across pkexec.
* **P3:** [polkit rules manual](https://www.freedesktop.org/software/polkit/docs/latest/polkit.8.html),
  authorization rules, subject properties, rule ordering, administrator identities.
* **N1:** [NixOS polkit module](https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/security/polkit.nix),
  `enablePkexecWrapper` and `security.wrappers.pkexec`. The Go toolchain was obtained
  from registry nixpkgs rev `1c89083e451e891152365b799595cf7790a43325`, not a lapdog
  build/activation; inspect lapdog's lock separately during integration.
* **S1:** [systemd-run manual](https://www.freedesktop.org/software/systemd/man/latest/systemd-run.html),
  `--pipe`, `--wait`, `--quiet`, `--collect`, service type and exit status.
* **S2:** [systemd.service](https://www.freedesktop.org/software/systemd/man/latest/systemd.service.html),
  RuntimeMaxSec, TimeoutStartSec, TimeoutStopSec, service types.
* **S3:** [systemd.kill](https://www.freedesktop.org/software/systemd/man/latest/systemd.kill.html),
  KillMode=control-group, SendSIGKILL, TERM then KILL.
* **S4:** [systemd run.c](https://github.com/systemd/systemd/blob/main/src/run/run.c),
  `ARG_STDIO_DIRECT` sets the three Standard*FileDescriptor D-Bus properties;
  `arg_wait && !arg_quiet` guards the summary; final status comes from
  `unit_result_to_exit_status` rather than an independent payload protocol.
* **S5:** [systemd manager D-Bus API](https://www.freedesktop.org/software/systemd/man/latest/org.freedesktop.systemd1.html),
  StartTransientUnit / StopUnit and job semantics; fallback reference, not a
  D-Bus implementation included in this proof.
