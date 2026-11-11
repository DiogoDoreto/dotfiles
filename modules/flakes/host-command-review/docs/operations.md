# lapdog operation and acceptance gates

## Status and deployment boundary

Integrated application and Nix packages, **not live desktop certified**. No host or
VM generation was activated, no actual pkexec authentication or transient service
was run, and no real browser was exercised here. Build/check results below are
not evidence that those runtime boundaries work. All deployment is manual.
Nothing in mini, Caddy or T3 is changed. There is no reviewer autostart. The only
scheduled job is cleanup-only maintenance (no listener/SSH/session advertisement).

## Before first use (human, on lapdog)

1. Review and build the lapdog host/Agent VM configuration; activate only through
   the user's normal deployment procedure. The package is Linux x86_64 only.
   Check `id -u dog` is **1000**; runtime refuses any other real/effective UID.
   We do not change the existing account UID. Inspect final policy ordering and
   wrapper/action binding before trying a root request.
2. Audit shares/mounts: `/home/dog/.local/state/agent-host-review` (0700, dog) and
   `/home/dog/.local/state/agent-host-review-pins` must NOT be guest-shared. The
   projects share and `/var/lib/agent-vm` must not contain host private captures,
   ledger, browser credentials or pins. The maintenance private key remains at
   `/var/lib/agent-vm/ssh/host/host_to_vm`, outside guest mounts.
3. Obtain the guest's **ed25519 host public key** and its SHA256 fingerprint over
   a trusted independent channel (console or trusted host access to the persistent
   backing `/var/lib/agent-vm/var/lib/ssh/ssh_host_ed25519_key.pub`; verify actual
   guest mount mapping). Do not trust `ssh-keyscan` or the existing
   `agent-vm-ssh` helper as proof: that helper disables host-key checking.
4. As dog, enroll a copied PUBLIC key using the independently verified fingerprint:

   ```sh
   agent-host-review --enroll /path/to/trusted-host-key.pub SHA256:VERIFIED_FINGERPRINT
   ```

   Enrollment accepts only an ed25519 public key with matching fingerprint. It
   creates the private pin directory and `known_hosts` (0600) exclusively, fsyncs,
   and **never overwrites** an existing pin. No silent TOFU/accept-new, system
   known_hosts, DNS verification or key update. To rotate a key, stop reviewer,
   independently verify the new identity, manually remove the old pin, then enroll
   again. Failed/partial enrollment requires inspection; it never silently repairs
   or overwrites a file. Normal startup refuses missing/unsafe pins.
5. Verify `/run/host-command-review-root` is root:root 0700 (tmpfiles creates it).
   The manual launcher creates `/run/user/1000/host-command-review` 0700; execution
   validates ancestors, actual ownership and runtime permissions again. Never
   replace immutable runner/helper paths with mutable development binaries.
6. In the guest, Home Manager invokes **Pi's own** `pi mcp add host-review` to merge
   only the reserved named entry in writable `~/.pi/agent/mcp.json`. It preserves
   unrelated servers and top-level settings, sets `deferred` exposure, and uses
   the immutable guest MCP executable. No whole-file symlink/template or Pi/T3
   extension. Restart Pi or `/reload`; `pi mcp list` should connect `submit`, `get`,
   `cancel`. Do not install a replacement `/mcp` extension. The guest queue is
   `/home/agent/.local/state/host-review-guest` (0700, agent).

## Manual session

Run `agent-host-review` in the **active local dog Plasma session**, not over SSH,
not as root and not as a service. The launcher verifies loginctl properties at
startup and every five seconds. It opens a fresh fragment-capability URL through
xdg-open; it does not print/log the URL. It prints only the routing session ID.
Give that ID to the guest agent; every tool call supplies `session_id` and a
fresh guest-chosen 64-lowercase-hex `request_id`. No reviewer means submit returns
`rejected/no_reviewer`. There is no queue for an eventual future reviewer.

Review the full frozen script/cwd/user/environment/runtime/digest and live-filesystem
warning. Save edits, then review the new revision. Root execution requires a fresh
KDE auth prompt for each consumed approval, with a 60-second auth timeout; no
password is entered in the browser. Only one command can be starting/running,
including the authentication interval. HTTP completion/retry is not permission
to rerun. An uncertain outcome is terminal; use a new request and deliberate review
if another attempt is appropriate, knowing side effects may already have occurred.

All results are held. Review the entire executed metadata and output, explicitly
mark script/cwd/stdout/stderr redactions, preview the complete export, and confirm
release using the in-page confirmation (not a blocking browser dialog). Keep the
tab visible so browser heartbeats continue. Nonce/expiry-only offer renewal preserves
redaction drafts, the preview and confirmation; edits invalidate the preview and
confirmation, and substantive artifact/authority changes block release until a
fresh review. Other metadata cannot be redacted in v1; withhold if sensitive. Repeated
SSH/MCP checks return identical published bytes and never run anything. Guest
status is coarse before release; execution errors and private diagnostics are not
forwarded. Guest timing still leaks coarse lifecycle timing.

End with the web End button or Ctrl-C. Shutdown blocks admission, cancels and joins
execution goroutines, revokes web liveness, publishes final coarse/released ledger
data while renewal continues where possible, closes the guest route, reaps SSH and
dbus-monitor, and deletes raw captures. A network loss may prevent final status
publication; it never grants a new approval or reconnects an expired route.

## Lifetimes and cleanup

* Host-renewed guest lease: 15 seconds, renewed on an independent five-second timer;
  fixed SSH process deadline four seconds, response/input byte caps. Lost renewal
  ends the session. Exchanges use only the fixed absolute executable with no remote
  arguments and JSON stdin; no script/path/ID interpolated into shell commands.
* Browser POST heartbeat: every three seconds while visible, advertised ten seconds;
  controller conservatively clamps to **five seconds**. Browser loss blocks new
  approvals/consume/release at expiry; already-consumed work may finish. Reload
  loses in-memory auth; restart the reviewer for a new bootstrap capability.
* Four-hour cap uses monotonic and suspend-inclusive clocks. dbus-monitor observes
  login1 PrepareForSleep, revoking epoch/browser lease on sleep/resume. The host
  also ticks every 250ms and detects clock discrepancies. Signals are asynchronous,
  not an acknowledged sleep inhibitor: actual suspend delivery/races are a live
  acceptance gate. Monitor death or inactive/nonlocal desktop ends the session.
* Execution: default 120s, maximum 1800s, independent systemd RuntimeMaxSec, two
  fresh start challenges, 500ms supervisor leases/2s expiry, five-second service
  TERM/KILL grace. Stop confirmation can take bounded additional transport time;
  manager failure yields uncertainty, not a real-time stop guarantee. Root can
  defeat its own containment; arbitrary reviewed command authority is intentional.
* Active-session GC runs every minute. Clean end deletes raw immediately; crash
  raw expires by 24h and ledger/snapshot/release at seven days from import. A
  cleanup-only dog oneshot runs every 15 minutes (persistent calendar timer),
  taking the singleton lock and recovering stale intents without replay; it deletes
  raw leftovers immediately. While a reviewer owns the lock it skips, leaving
  that reviewer's sweep responsible. No session, browser, SSH or execution is
  started by `--cleanup`. Seven-day ledger expiry is observed at the next sweep;
  offline/suspended machines catch up on wake/start. A stopped/disabled timer or
  corrupt/poisoned store can prevent cleanup and must be investigated; no promise
  of deleting bytes on a powered-off disk. Filesystem deletion is not secure erase.
* Queue bounds are deliberately small: eight guest retained requests, sixteen
  guest routes and 256 host ledger entries by default. Backpressure is explicit,
  not silent loss. Retention does not extend on polling. Disk corruption/fullness
  fails closed. Do not remove lock inodes or edit the live store to clear a queue.

## Packaging verification performed here

The lapdog-pinned nixpkgs polkit module was inspected: `enablePkexecWrapper` is an
explicit option, and `security.wrappers.pkexec.enable` follows it. Evaluated wrapper:
root:root, setuid true, source the pinned **polkit-127** executable, exposed at
`/run/wrappers/bin/pkexec`. `environment.pathsToLink` includes `/share/polkit-1`.
The package installs the dedicated action with exact store runner path and argv1
`--approved-sha256`; lapdog installs an early `00-host-command-review.rules` rule
requiring local active dog and target root/program exact path, AUTH_ADMIN (never
KEEP), with dog-only admin identity for this action. Generic pkexec authority is
not globally removed; runner invocation validation remains mandatory.

Both host and guest `buildGoModule` outputs built successfully, including tests.
Exact store paths and UID are injected into binaries through linker strings;
runner/helper self references use their own immutable output, not mutable wrappers.
The standalone package and lapdog-followed package can have different store paths:
only the latter's action/policy may be used for lapdog runtime validation.

Native Pi **1.5.0 from lapdog's pinned llm-agents input** was built and exercised
with a temporary private HOME/config, twice replacing the owned entry while
preserving an unrelated disabled server and a top-level setting. `pi mcp list
--json` connected all three real Nix-built guest tools. Reproduce with:

```sh
python3 modules/flakes/host-command-review/tests/pi-native-smoke.py \
  /nix/store/...-pi/bin/pi /nix/store/...-guest/bin/host-review-mcp
```

This is a real Pi config/connection smoke test, not a real Pi conversation, desktop
session, full-size Pi rendering test, or activation. Go subprocess SDK tests cover
full-size MCP releases independently.

## Pending live gates — do not call this ready for deployment

All ten gates in [probe/FINDINGS.md](../probe/FINDINGS.md#live-lapdog-desktop-acceptance-gates--all-pending)
remain pending. In particular:

1. Real wrapper/action selection and effective rule order; every adjacent root
   command gets a new KDE prompt; wrong caller/session/target/argv rejection,
   denial/timeout and no internal text agent/cached grant.
2. Actual systemd --pipe stdio FD passing, trusted bash FD3 semantics, user-manager
   analogue, root identity/groups, unit activation/collection, late submission,
   stop bus races, and independent runtime after reviewer/runner SIGKILL.
3. Descendant cgroups (setsid/TERM resistance), floods/held descriptors, lost
   terminal/stop confirmation, helper/CLI death and journal/process metadata audit.
4. Actual browser CSP/origin/history/visibility/throttling/multi-tab behavior,
   fragment URL handling by desktop opener, auth loss/reconnection and live suspend
   before/after both start gates; filesystem mount/export and fsync acceptance.
5. Host/guest deployed-generation paths, dog UID, private permissions and timer
   operation on the real machine. Pin enrollment and a real pinned-key SSH roundtrip
   were not performed in this isolated implementation environment.

Independent code review must also inspect these joins and the explicit live gates;
passing mock/unit tests is not a substitute.

## Integration validation record

Passed in the isolated implementation environment:

* Main module `go test ./...`, `go test -race ./...`, `go vet ./...`.
* Separate `probe/` module: the same test/race/vet commands.
* Five-second/two-worker `FuzzRequest` and `FuzzEnvelope` smoke runs.
* `node --check web/assets/app.js` and `node --test web/assets_test.cjs` (four tests).
* Launcher tests: local desktop admission, independent fingerprint validation,
  sleep-monitor invalidation/reaping, cleanup skipping a live singleton and
  deleting crash raw. Integration tests include independent renewal with a stuck
  pull, closed routes, active shutdown, browser expiry at nonce consumption and
  full held/redacted-release lifecycle with identical retries.
* `make format-nix` (only task Nix files changed; no unrelated formatting).
* `nix flake check ./hosts/lapdog/` (lapdog and Agent VM) and standalone reviewer
  flake check. Existing warnings concern SSH defaults and deprecated PipeWire JACK.
* `nix build --no-link` for standalone host/guest outputs **and** the exact reviewer
  packages selected from lapdog/Agent VM `environment.systemPackages`.
* Native Pi smoke script above with pinned Pi 1.5.0 and the deployment's guest
  package. Cleanup calendar syntax also validated with `systemd-analyze calendar`.

Full NixOS toplevel closures were not built; these were targeted package builds
and full host-flake evaluation checks. No deployment/activation, real pinned-key
network exchange, browser session, root payload, GUI authentication or transient
systemd unit was run. Those are genuine remaining runtime gates, not evidence
provided by the fake execution backend.
