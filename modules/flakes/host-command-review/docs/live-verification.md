# Live lapdog verification record

Historical evidence below uses the former MCP `submit`/`get`/`cancel` interface.
It does not verify the current `propose_command`/`get_command_status`/
`request_cancellation` interface or its required reason. See
[the current interface](command-interface.md); no new live deployment was performed.

## Scope and evidence

Interactive verification on lapdog, with the human operating the local dog
Plasma/Wayland session and a coding agent inside lapdog's Agent VM. The user
manually activated configurations; the agent did not activate any generation.

This records the tests performed during the implementation-verification
conversation, through commit `504e2a2`. It is **partial acceptance evidence, not
full desktop certification**. User-observed UI behavior is identified separately
from released artifacts and automated tests. No browser capability URL, browser
authentication token, password, maintenance private key or private capture is
recorded here. Routing/request IDs below are identifiers, not approvals.

The original isolated build/check record in [operations.md](operations.md) still
applies to that earlier implementation environment. Live observations here do not
retroactively turn its mocks into systemd/polkit proofs. No independent code-review
signoff was performed in this conversation.

## 1. Deployment, UID, permissions and cleanup scheduling

**Observed on lapdog by the user:**

- Configuration activation reported complete.
- `id` returned UID 1000 for dog, primary GID 100 (`users`), with supplementary
  wheel/audio/video/networkmanager/kvm membership.
- The launcher and `agent-vm-ssh` were installed in the running system profile.
- `/run/host-command-review-root` was `root:root 700`.
- `host-command-review-cleanup.timer` was enabled, active/waiting and scheduled.
- Before first use, reviewer state and pin directories did not exist, as expected.
- `/var/lib/agent-vm/ssh/host` was `dog:users 700`.

Commands included `id`, `readlink -f /run/current-system`, `command -v`, `stat`,
`systemctl status host-command-review-cleanup.timer`, and
`systemctl cat microvm@agent-vm.service`.

**Limitations:** timer scheduling was checked, not an actual successful cleanup
run. Runtime user-directory ancestry, stale-store recovery and fsync semantics
were not exhaustively exercised live.

## 2. Guest mount boundary and trusted host-key enrollment

**Observed:**

- Host `current` and `booted` VM launchers resolved to the same immutable output.
- Host `virtiofsd` process arguments exported only the expected store, guest home,
  guest `/var/lib`, guest logs, guest SSH directory and projects directories.
- Guest `findmnt` showed corresponding tags/mounts: `ro-store`, `agent-home`,
  `agent-var-lib`, `agent-var-log`, `agent-ssh`, and `projects`.
- No inspected export included the host reviewer state/pins, the maintenance
  private-key directory, or `/var/lib/agent-vm` as a whole.
- Host `/var/lib/agent-vm/var/lib` mapped to guest `/var/lib`, confirming the
  persistent guest sshd public-key backing location.

The user obtained the ed25519 public-key fingerprint through trusted local host
access, not SSH keyscan or the host-key-checking-disabled convenience helper:

```sh
sudo ssh-keygen -lf \
  /var/lib/agent-vm/var/lib/ssh/ssh_host_ed25519_key.pub -E sha256
```

Verified public fingerprint:

```text
SHA256:visfafCJlb6YX4wfoy44SfbHDB1DmcsC1v19ncVYlQY
```

The user copied only the public key into a host-runtime temporary file and
successfully enrolled it as dog with `agent-host-review --enroll`. They confirmed
pin-directory mode 700 and `known_hosts` mode 600, with correct ownership.
Subsequent real reviewer/guest exchanges succeeded with this enrollment.

**Limitations:** these process/mount observations are not a comprehensive audit
of every possible bind mount, filesystem alias or future export change. Key
rotation, unsafe pin rejection and failed/partial enrollment were not tested live.

## 3. Native Pi MCP integration and deployed-generation checks

Inside the VM, `pi mcp list --json` reported the native `host-review` entry enabled,
with deferred exposure, connected `submit`, `get`, `cancel` tools and no errors.
Real tool calls were used throughout subsequent tests.

A later deployment mismatch was caught rather than bypassed: both the running
guest profile and Pi configuration still pointed to the original guest output
(`0db6rgc6ym8lkj0saf7j02q09mffwwbh-…`) after host-side capacity fixes were deployed.
A new reviewer session still returned `queue_full`. After the user updated the
guest/reloaded the connection, the installed guest executables and Pi configuration
pointed to `vg1cqdzjhi3s6dimlma2q9391s0izlhy-…`, and a fresh submission returned
`pending` with the existing history retained.

**Limitations:** `pending` is guest routing, not host acceptance or execution.
This final admission check does not prove all new capacity boundaries live.

## 4. Desktop admission failure and correction

The first reviewer launch failed with:

```text
host reviewer stopped: active local dog Plasma session required
```

The user was running bash in Ghostty in an active, local KDE Wayland session.
Separate `loginctl` property arguments showed `Name=dog`, `Active=yes`,
`Remote=no`, `Class=user`, `Type=wayland`, `Desktop=KDE`.

The launcher's exact comma-separated query returned no properties with status 0:

```sh
loginctl show-session "$XDG_SESSION_ID" \
  --property=Name,Active,Remote,Type,Class,Desktop
```

Commit `6e7f433` changed the query to repeated `--property` arguments and tightened
the subprocess fixture to check the arguments. The test reproduced the rejection
before the fix, then passed. After user deployment, the reviewer launched and
opened its browser successfully. No desktop admission check was weakened.

## 5. Unprivileged execution, editing and held-output boundary

Request:
`2a5862fc2068499295a2e5e7904d304e167766f58602f05745caa41c35f49f00`.

- Submitted as dog, cwd `/`, timeout 30 seconds.
- Script printed a revision marker, `id`, cwd and separate stdout/stderr markers.
- User saw the request in `reviewing`, changed `review-smoke-v1` to
  `review-smoke-v2`, saved, reviewed and approved it. No auth prompt was reported.
- Before release, guest `get` returned only `held`, with no artifact or output.
- After release, the artifact showed revision 2, the edited script, UID 1000 dog,
  GID 100 users, expected supplementary groups, cwd `/`, exit 0 and no truncation.
- Two retrievals returned identical released content and the same artifact ID.

The first released artifact was **not redacted**: it contained the stdout test
marker and `redacted_fields: []`. The user subsequently reproduced a UI refresh/
confirmation problem that could lose their draft. This release was not counted
as successful redaction evidence. Markers were nonsecret test data.

## 6. Redaction, offer renewal and confirmation race

A second marker-only dog request,
`84408f0e498178fb277a6de2253499ecd554f9e5b11bff9b9f6262e10777a8e0`,
was quickly redacted/released. The guest received `[REDACTED]` in stdout and
`redacted_fields: ["stdout"]`. The approved script still contained the marker,
as expected: only stdout had been redacted.

A third request,
`e145bfb9254aa41d02c7a04dec32ef1e5f2930ec42c06b865c64a6e65003039f`,
was used to reproduce the UI issue. The user observed:

1. After Preview, a periodic update cleared the preview and disabled Release.
2. A renewal/update during native release confirmation could leave the form
   unusable; selecting the request again discarded the redaction draft.

This request was left unreleased. No released artifact was modified in place.

Commit `0928f23` preserved previews/drafts across nonce/expiry-only renewal and
replaced native release confirmation with an in-page confirmation. Confirmation
freezes the previewed artifact; edits and substantive authority/artifact changes
still invalidate release. Native dialogs blocking heartbeat was a plausible
contributor, not independently proven browser behavior.

**Live post-fix check:** request
`bf5ea05ecd1280a6d5c4334d0f3e23553fb63c5ab75ff18f1832b2f4b9f54967`.
The user reported completing both 40-second waits: one after Preview and one with
the in-page confirmation open, keeping the tab visible. Both cross the default
30-second offer lifetime. After confirmation, the guest received:

```text
renewal-check-visible
[REDACTED]
```

The artifact reported stdout redacted, exit 0, no truncation. Repeated retrieval
returned identical content/artifact ID. This verifies the reported race scenario
after the fix, not all browser/throttling/multi-tab/suspend behavior.

## 7. Installed wrapper, action and rule inspection

Read-only dog request:
`c873f73821a111572dd68f9a77caaa7a0450560f898f82c87f8383684ed2cdfa`.
The human inspected and released the output; exit 0, stderr empty.

Inspection included the installed reviewer/runner paths, `stat`, dedicated policy
XML, `pkaction --action-id br.com.doreto.host-command-review.execute --verbose`,
complete `.rules` files in the inspected system rule directories and
`systemctl cat polkit.service`.

Findings at that deployment:

- Runner was immutable-store `root:root 555`; pkexec wrapper `root:root 4511`;
  root runtime directory `root:root 700`.
- Policy XML, registered action annotations and early rule matched the same exact
  installed runner path (`xqflgcij2bycv5jzqmbfnygwxkkm0hzv-…/bin/host-command-root-runner`).
- Action argv1 binding was `--approved-sha256`; active default `auth_admin`, other
  defaults `no`; no cached grant requested.
- `00-host-command-review.rules` preceded the other inspected rules, checked
  active/local dog, exact program and root target, and selected dog-only admin
  identity. No earlier overriding rule was present in the inspected directories.
- Service configuration selected polkit 127.

**Limitations:** static/registered-action inspection does not by itself prove
runtime action selection or every configured search directory. The wrapper's
underlying executable binding was not independently inspected live. Later package
rebuilds change exact store paths and require matching installed policy bindings.

## 8. Two separately authenticated root executions

Requests:

- `5e1c54f9442198d4c3cf8a46da11689b24e93e3862fab80cb80454a309a6e4a1`
- `b39fc363d414b60093e16e982f61d177402735e8b90dfb09105aad76e919ee3d`

Each was a distinct approval of this identical read-only script, cwd `/`, timeout
30 seconds:

```sh
printf 'root-identity-check\n'
/run/current-system/sw/bin/id
printf 'cwd='; pwd
```

The user reported the expected KDE prompt for the first and a fresh KDE prompt for
the second. Each released artifact showed:

```text
root-identity-check
uid=0(root) gid=0(root) groups=0(root)
cwd=/
```

Both exited 0, with empty stderr and no truncation. Their execution digest matched
but request/artifact IDs differed. This is evidence of fresh authentication for
these two approvals, not universal proof against caching. Exact runtime action-ID
details were requested but were not explicitly reported by the user.

**Limitations:** successful payloads indirectly exercise real root transport, but
do not directly prove every FD, environment, unit property, collection, cgroup,
independent runtime or metadata-isolation requirement.

## 9. Prompt cancellation: conservative uncertain outcome

Request:
`b21943db5c8a8a0430009230b72d2c55d6fed964389cb8ef10f26f9561a2d3e5`.

Marker-only root script: `printf 'AUTH-CANCEL-TEST-PAYLOAD-RAN\n'`.
The user promptly cancelled the KDE prompt. The reviewer became `ambiguous`,
with no captured stdout and no further actions offered.

A separate user-run diagnostic in the local desktop terminal invoked installed
pkexec/runner with a syntactically valid all-zero digest and **empty stdin, no
manifest or payload**. The user cancelled promptly again and reported pkexec
exit **127**.

The implementation recognizes pre-runner-ready 126 as authentication cancellation;
127 is deliberately uncertain because it can also represent authorization failure
or pre-ready runner failure. No blanket 127-to-cancelled mapping was added.

**Result:** terminal, conservative handling observed. Distinct reliable auth-cancel
classification remains unverified on this KDE/pkexec combination. Absence of stdout
alone is not proof of no execution. Wrong credentials, absent auth agent and
terminal-agent fallback were not tested.

## 10. Backpressure, stale deployment and capacity fixes

After eight retained guest requests, an attempted timeout submission returned
`rejected/queue_full`. A new reviewer session also returned `queue_full` while the
old guest executable remained deployed. Neither rejected attempt entered host
review; neither was a timeout test.

Commit `504e2a2` separates admission from history on both sides:

- Guest: 100 nonterminal requests per session; terminal history consumes no work
  slots; 16 live routes excluding closed-route tombstones; independent 900 MiB
  aggregate state-byte cap.
- Host: 100 outstanding requests in the current session; previous-session and
  terminal history consume no work slots; independent 2 GiB file/name byte budget,
  including write peak; no 256-record admission ceiling; directory enumeration
  uses a 16 MiB name-byte budget rather than a 4096-entry ceiling.
- Neither side silently evicts retained results to admit new work.

**Live post-update evidence:** existing guest history did not prevent a fresh
submission from returning `pending`. Host UI acceptance, 100/101 boundaries,
large-history recovery and byte-budget exhaustion are covered by automated tests,
not yet exercised as live deployment acceptance gates.

## Automated regression validation performed during this conversation

- Desktop-query regression: failed on the original argument form, passed with
  separate property arguments.
- UI regression: reproduced preview invalidation and blocking native confirmation;
  all 12 final dependency-free DOM tests pass, including preserved redactions,
  non-modal confirmation across renewal, edit invalidation, and substantive
  artifact/authority changes blocking confirmation.
- Guest regressions: 100 admitted/101st rejected; terminal history frees slots;
  independent session admission; closed routes preserve tombstones without using
  live-route slots; live-route limit still enforced; oversized-state rejection.
- Transport: 100 large proposals parse successfully; 101 proposals rejected.
- Host regressions: terminal/previous-session history does not consume slots;
  held results consume slots until release; immutable released bytes survive new
  admissions; history/recovery beyond 256 records; directory scanning beyond
  4096 entries; byte-budget rejection preserves data and leaves healthy store
  usable. Existing recovery/fault-injection tests also pass.

Final validation commands (main module):

```sh
go test ./...
go test -race ./host ./guest ./transport ./reviewer
go vet ./...
node --check web/assets/app.js
node --test web/assets_test.cjs
```

`git diff --check` passed before the fix commits. Go commands ran via
`nix shell nixpkgs#go` in the Agent VM. No new full NixOS closure build, fresh Nix
flake check, probe-module rerun or fuzz run was performed by the agent during
these fixes; earlier packaging checks remain a separate record. The user handled
builds/deployment, with guest-path observations as recorded above.

## 11. Authentication timeout — observed, conservatively uncertain

The user resumed this test after the initial verification-record commit
`b556412`. The fresh marker-only root request was:

```text
session_id: 4b2a3788b16e0a497480d1770664b85bc5f6fcc7e4dd0ba4416c1ef2868badf7
request_id: f5bd3fb57a920d7df5b303aa7a0294975ac704eed2d90a5bfc508f9f55bbc2d8
observed final guest status: uncertain
```

Script: `printf 'AUTH-TIMEOUT-TEST-PAYLOAD-RAN\n'`, root, cwd `/`, 30-second
execution timeout. The procedure was to approve, leave the KDE prompt unanswered,
keep the browser visible and allow time for the separate 60-second authentication
timeout plus bounded reaping.

**User-observed:** KDE's prompt timed out after 60 seconds; the reviewer status
became `ambiguous`. **Guest-confirmed:** a subsequent `get` returned only
`uncertain`, with no artifact, output or private diagnostic information.

**Result:** prompt timeout and conservative terminal handling were observed for
this attempt. A distinct reliable authentication-timeout classification was not
established. No captured-output inspection or independent payload/process/unit
absence check was reported; coarse status alone is not proof that no payload
executed or that all descendants/transport processes were reaped. Do not retry
this consumed request. Reviewer-death/delayed-authentication races remain separate
untested gates.

## Remaining acceptance work

None of the ten broad [probe acceptance gates](../probe/FINDINGS.md#live-lapdog-desktop-acceptance-gates--all-pending)
has complete signoff. The evidence above covers selected parts only. Still pending:

- Complete authentication-timeout cleanup/no-payload evidence (the 60-second
  prompt timeout and terminal uncertain result were observed); wrong credentials/
  absent agent; wrong caller/session/target/program/argv and malformed manifest
  rejection; explicit runtime action-ID observation and no internal text-agent
  fallback.
- Direct systemd FD3/stdin/stdout/stderr and clean environment checks; payload
  126/127 and signal/failure classification; start/collection/bus races.
- Cancellation/timeout of process trees including setsid/TERM-resistant children,
  output floods/held descriptors, truncation and bounded drain/stop confirmation.
- Reviewer/runner/helper/CLI death or freeze, especially around authentication and
  both start gates; independent RuntimeMaxSec and absence of orphan listeners.
- Browser origin/CSP/history/visibility/throttling/multi-tab behavior, bootstrap
  fragment handling, browser loss and reload, suspend/resume race acceptance.
- Live process/unit/journal metadata audit, actual cleanup/retention operation,
  disk/fsync/failure tests, and live new capacity boundaries.
- Independent code-review signoff. Unit/mock tests are not substitutes for these
  runtime boundaries.
