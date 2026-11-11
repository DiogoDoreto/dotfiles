# Manual lapdog host-command reviewer — integrated, not live-certified

The Agent VM proposes commands through native Pi MCP. A **manually started**
foreground `agent-host-review` in dog's local active Plasma session pulls proposals
using strict-pinned SSH, imports immutable host snapshots, and opens an authenticated
loopback web reviewer. Only that host reviewer can approve execution or release
results. Guest root controls all guest data; routing IDs are not credentials.

The application seams are connected and tested with fake SSH subprocesses, real
HTTP, the actual guest queue and host ledger, and a mock execution backend.
Immutable Nix packages and lapdog host/guest wiring are included. **This is not
approval to activate or a claim that real desktop execution is certified.** All
systemd/polkit/KDE/browser acceptance gates remain pending; see
[operations](docs/operations.md), [execution](execution/README.md) and
[the feasibility proof](probe/FINDINGS.md). No deployment was performed.

## Components and authority

| Component | Responsibility |
| --- | --- |
| `contracts/` | Strict closed v1 JSON, proposal, snapshot, frozen spec/digest, approval, coarse reply and immutable release |
| `guest/`, `transport/`, `cmd/host-review-{mcp,exchange}` | Bounded guest queue, 15-second host-renewed routing leases, fixed bounded JSON exchange, official Go MCP SDK stdio; no execution authority |
| `host/` | Host-private snapshots/ledger, revisions/epochs/nonces, durable consume before launch, one command, crash ambiguity, explicit release and retention |
| `execution/`, `cmd/host-command-{root-runner,unit-helper}` | Fresh two-stage start gates, private framed stdio and leases, pkexec for root, systemd transient services, FD3 script, bounded capture/cleanup |
| `web/` | Authenticated loopback HTTP + embedded plain-text UI, browser liveness, full execution/release preview, no frontend dependency |
| `reviewer/` | Backend adapter and strict SSH client/worker; this is the only join between host authority, browser and untrusted guest |
| `cmd/agent-host-review` | Manual foreground launcher, trusted runtime config, local Plasma checks, deliberate pin enrollment, sleep monitor, retention timer and shutdown/reaping |
| `flake.nix` | `packages.x86_64-linux.{host,guest}`, `lib.mkHost`; immutable runner/helper/tool paths and UID injected at build time |
| `hosts/lapdog/{reviewer-host.nix,vms/agent-vm/reviewer-guest.nix}` (repository root) | Dedicated noncached polkit action/rules and root runtime; guest package and Pi-owned named MCP entry merge |

No reviewer autostart, public listener, Caddy route, T3 plugin, persistent privileged
helper, sudo fallback, or mini change. The SSH maintenance key is reused, but **not**
the maintenance helper's disabled host-key checking. A separate deliberately
enrolled pin file is required. All scripts/cwd/env and output remain host-private
until an explicit immutable reviewed release; transport errors are fixed coarse
codes, never raw host diagnostics.

## Fixed unreleased v1 policy

* Four-hour session maximum including suspend; one execution including root auth.
* Default runtime 120 seconds, maximum 1800; five-second service TERM/KILL grace.
* Fixed store bash `--noprofile --norc /proc/self/fd/3`, unlinked immutable script
  FD3, stdin `/dev/null`, no PTY/startup files/inherited environment. This explicitly
  amends unreleased v1's former `-c` wording; digest encoding is unchanged.
* Combined 4-MiB stdout/stderr retention, concurrent drain/discard after cap.
* Exact current snapshot/revision/digest/epoch/nonce and browser lease checked
  before durable nonce consumption; retries and recovery **never reexecute**.
* Raw capture deleted on clean end; crash raw expires after 24 hours; ledger and
  released artifacts after seven days. Recovery/startup, active-session periodic
  sweep and a cleanup-only 15-minute timer enforce age policy. No reviewer session
  is autostarted; see operations for offline/cleanup failure limitations.
* Byte freezing is not a filesystem snapshot or sandbox. Reviewed arbitrary root
  commands can defeat their own systemd limits; no malicious-root containment.

Exact JSON bounds, canonical digest encoding and parsers are documented in source
and component READMEs. IDs/nonces are random 32-byte lowercase hex; guest IDs are
untrusted data, never host paths. Before release, guest status carries no executed
metadata, output, exit info, host errors or approval capabilities. Redaction is
limited to script/cwd/stdout/stderr with explicit markers; sensitive non-redactable
metadata means withhold the entire artifact.

## Dependencies

Go 1.25+; tested using Go 1.26.10. Host, transport and execution use the standard
library. Only `guest/mcpstdio` imports the official MCP SDK, pinned to v1.8.0;
[guest/README.md](guest/README.md) records the dependency assessment and actual
import graph. No custom MCP stack, SQLite/CGO, web framework, frontend package
manager, or handwritten D-Bus. Nix `buildGoModule` pins the vendor hash.

## Validation

From this directory, using `nix shell nixpkgs#go --command ...` if needed:

```sh
go test ./...
go test -race ./...
go vet ./...
(cd probe && go test ./... && go test -race ./... && go vet ./...)
node --check web/assets/app.js
node --test web/assets_test.cjs
```

From the repository root:

```sh
make format-nix
nix flake check ./hosts/lapdog/
nix build --no-link ./modules/flakes/host-command-review#host ./modules/flakes/host-command-review#guest
```

Component READMEs provide focused tests; integration coverage includes a complete
fake-SSH → guest → host → authenticated web → mock executor → held → redacted
release lifecycle, unchanged publication retries, stale revisions/epochs/nonces,
release metadata tampering, browser loss at consumption, cancellation, active
shutdown/join, closed guest routes and malicious/blocked/oversized SSH peers.
These tests deliberately do not run privileged payloads or real transient units.
