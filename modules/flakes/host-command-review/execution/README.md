# Per-command execution transport — implemented, NOT live-certified

Go stdlib only. Production path is `os/exec` → `systemd-run --pipe --wait`
transient service → immutable in-unit helper. There is **no process-group
production fallback**, sudo, persistent privileged daemon, or VM-accessible
listener. Unavailable manager/wrapper/config means fail closed/uncertain, never
execute by another mechanism. Tests do not establish real systemd/polkit behavior.

## Trusted packaging configuration

Both `cmd/host-command-root-runner` and `cmd/host-command-unit-helper` have EMPTY
build-time defaults. They exit 70 without validated configuration. The integrated
Nix packaging injects these Go linker strings consistently into both binaries:

```
-X main.shell=/nix/store/.../bin/bash
-X main.systemdRun=/nix/store/.../bin/systemd-run
-X main.systemctl=/nix/store/.../bin/systemctl
-X main.helper=/nix/store/.../bin/host-command-unit-helper
-X main.runner=/nix/store/.../bin/host-command-root-runner
-X main.rootRuntime=/run/host-command-review-root
-X main.userRuntime=/run/user/UID/host-command-review
-X main.expectedUID=ACTUAL_DOG_UID
```

Do not accept these settings from browser, guest, environment or CLI. Runtime
directories must already exist private 0700, root/dog owned respectively; helper
walks all ancestors with O_NOFOLLOW and rejects writable-by-others ancestors.
Anonymous script files are created directory-relative 0600 and immediately
unlinked; fd3 alone is inherited by bash. Runtime setup belongs to packaging /
manual launcher, not a privileged mkdir on guest-supplied input. The integrated
single-output Nix package injects its own exact output paths for runner/helper;
neither mutable symlinks nor development binaries are accepted.

Runner verifies actual executable equals trusted store path and real/effective
UID. Root mode requires real/effective root plus exact `PKEXEC_UID` equal to
configured dog UID. User mode requires exact real/effective dog UID. Helper
independently checks its own executable, spec target UID and root GID/groups.
These checks do not authenticate a hostile root or malicious same-UID reviewer.

Root client uses ONLY `/run/wrappers/bin/pkexec`, requiring a root-owned setuid
non-writable regular wrapper, with:

```
pkexec --disable-internal-agent FIXED_RUNNER --approved-sha256 DIGEST
```

Packaging MUST install and inspect the dedicated action
`br.com.doreto.host-command-review.execute`, bound to EXACT store runner path and
argv1 `--approved-sha256`, active/local dog → root, `auth_admin`, never `_keep`.
See `../probe/FINDINGS.md` for policy XML/rule sketch. Integration includes lapdog policy/Nix wiring, but no activation was performed. Code alone cannot prove which polkit action won rule ordering or
prevent a mispackaged generic-action fallback. This is a hard live release gate.
One pkexec child per consumed command, including adjacent identical requests.
Auth lifetime is 60 seconds; pipe writing is bounded too, including large
manifests while GUI auth is pending. Cancellation before READY closes control
and kills/reaps the pkexec child. An auth response arriving later cannot pass START.

User execution invokes the same runner with `--user --approved-sha256 DIGEST`,
using analogous `systemd-run --user` / `systemctl --user`. CLI environment is
closed, with only fixed PATH/LANG/HOME and dog XDG_RUNTIME_DIR; no inherited bus
or shell startup variables. Verify actual user-manager accessibility on lapdog.

## Private protocol / gates

Linux inherited stdio is reconstructed as deadline-capable nonblocking private
pipes; errors establishing deadlines fail closed. Frames are a big-endian 4-byte
length plus exact canonical closed JSON `{type,data}` (data base64), bounded 8MiB
before allocation. Manifest is the exact canonical `ExecutionSpec` JSON, bounded
512KiB, SHA256-bound to approval digest and validated independently in both roles,
including exact trusted bash, closed environment, identity, capture, timeout/grace.
This is a host-private execution protocol, not the guest worker's envelope.

1. Client sends manifest only, never prebuffers START during GUI authentication.
2. Runner validates identity/manifest then emits fresh 32-byte `runner_ready`.
3. Client checks context and echoes `start` challenge; only then can submission
   begin. Client renews empty `lease` frames every 500ms, 2s expiry.
4. Runner creates fresh private unit pipes, passes only helper path, digest,
   random unit name and fixed properties in argv. It sends manifest to helper.
5. Helper revalidates manifest/identity and emits a NEW `unit_ready` challenge.
6. Runner sends `unit_start` only after receiving it and while reviewer context
   remains live. No second START is buffered before UNIT_READY. Runner renews
   helper leases on the same 500ms/2s policy. Missing/partial/wrong frame, CANCEL,
   EOF or lease expiry stops the lifecycle; partial reads do not renew leases.

Both fresh gates have five-second deadlines. A death immediately after a valid
START still races execution: EOF/leases and independent RuntimeMaxSec handle the
remaining ordinary-process lifetime; no race-free “alive at next instruction”
claim. Root can defeat its own manager/cgroup limits; **no hostile-root containment**.

Transient unit has Type=exec, RuntimeMaxSec=reviewed timeout (default120s,
maximum1800s), TimeoutStartSec=5s, TimeoutStopSec=5s, KillMode=control-group,
SendSIGKILL=yes, Restart=no, UMask=0077, --collect, no environment expansion.
Helper prestart wait is inside independent RuntimeMaxSec too. Stop uses bounded
8s `systemctl stop UNIQUE_UNIT`, not just killing systemd-run. A bus/submission
race is uncertain; late helpers receive EOF/no second gate. Unit names never
repeat or replace existing services. Stop confirmation without a framed payload
terminal is still uncertain; no inferred success from an exit code/PID.

Helper starts fixed bash, no PTY/startup files, exact reviewed cwd/environment,
stdin `/dev/null`, script fd3. Separate concurrent stdout/stderr capture shares
4MiB budget; excess is discarded while draining. Main-process completion allows
one second bounded drain, marks truncation if descendant-held pipes require
closure, then helper exits and systemd deactivation cleans the remaining cgroup
with the five-second grace. Runner requires both valid helper terminal frame and
successful `systemd-run --wait` deactivation. A killed/timed-out helper without
terminal frame yields uncertain, not an invented timed_out/cancelled exit. A
valid racing normal exit may win cancellation. Missing/truncated/malformed or
extra terminal data is not success. Payload exit126/127 is framed, not auth.
Transport diagnostics are discarded, never mixed into payload framing/output.
All framed results remain host-private until explicit controller Release.

Pre-runner-ready pkexec126 is classified as auth cancellation under the runner's
reserved exit convention (runner voluntarily uses only0/70). Pre-ready127 is
ambiguous, not proven auth denial. After READY, missing terminal is execution
loss. After terminal, a failing runner/CLI also conservatively becomes uncertain.

### Unreleased v1 invocation policy amendment

Integration reconciles contracts, UI and state-machine documentation in favor of
`bash --noprofile --norc /proc/self/fd/3`: immutable unlinked script FD3, never
script text in argv or environment. v1 has not shipped; its fixed invocation
policy is amended without changing digest struct encoding or protocol number.
`$0`/`BASH_SOURCE` identify `/proc/self/fd/3`; this is not `bash -c` semantics.
Byte freezing does not freeze mutable filesystem semantics.

## Tests / limitations

```
go test -race ./host ./execution ./cmd/host-command-root-runner ./cmd/host-command-unit-helper
go vet ./host ./execution ./cmd/host-command-root-runner ./cmd/host-command-unit-helper
```

Fake backend and actual unprivileged child-process tests exercise outer fresh
gate/EOF/partial lease, nested full4MiB result framing, payload126/127, no stdin /
no inherited BASH_ENV, bounded combined capture/drain, main-process timeout,
inherited pipe deadline support, fake CLI success/lost terminal/nonzero status,
delayed submission cancellation and second challenge. The command-factory seam
is private to package tests. No test invokes real systemd/pkexec or root payloads.
No live cgroup/FD-passing semantics are inferred from these fakes.

ALL ten live lapdog desktop acceptance gates in `../probe/FINDINGS.md` remain
pending. In particular: actual wrapper and dedicated no-cache action selection;
per-command Plasma prompts/denial/60s auth timeout; root caller/groups; actual
systemd --pipe FD passing and collection; late service creation/stop bus races;
reviewer/runner/helper death before/after both gates; independent runtime after
SIGKILL; setsid/TERM-resistant descendants; output FD retention/flooding; process
argv/unit metadata/journal audit; user-manager analogue. Need exact deployed
paths, shell fd behavior, private runtime setup and filesystem fsync acceptance.
Integration now supplies the authenticated UI adapter, guest transport wiring,
joined session shutdown, sleep monitoring and retention scheduling. See
`../docs/operations.md`; actual desktop/suspend semantics remain live gates.
