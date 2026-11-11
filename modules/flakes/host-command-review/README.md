# Host command review — foundation only

This directory is a **Go standard-library-only contract library**, not a usable
reviewer. There are no entry points, SSH/MCP/HTTP transports, store, root helper,
polkit policy, service, UI, or deployment changes. Nothing can execute a command.
No new flake/package is supplied until there is a binary worth packaging; later
packaging belongs here, not in a host configuration. No mini changes or activation.

The intended deployment is **lapdog only**, manually started by `dog` from a local
Plasma session as `agent-host-review`. The Agent VM can propose/check/cancel;
only the host reviewer can authorize execution or release results. Guest root
controls all guest data. A session ID routes requests; it never authenticates them.

## Files and ownership boundaries

| File / future package | Responsibility |
| --- | --- |
| `contracts/types.go` | v1 proposal, fixed remote request, frozen execution, approval binding and digest |
| `contracts/parse.go` | bounded, exact, closed JSON parsers; no I/O other than supplied reader |
| `contracts/snapshot.go` | immutable host import and provenance; separate from editable execution spec |
| `contracts/result.go` | deliberately coarse reply and explicit immutable release artifact |
| `contracts/state.go` | host phase transition **shape**, not authorization or persistence |
| `contracts/testdata/` | executable v1 wire fixtures (not authority-bearing credentials) |
| `docs/state-machine.md` | guards, crash recovery, lifetimes and output policy |
| `docs/storage-and-workers.md` | proposed file transaction model and worker seams |
| future `internal/guest`, `cmd/agent-host-review-mcp` | MCP adapter, bounded guest queue; no execution authority |
| future `internal/host`, `cmd/agent-host-review` | pinned SSH pull, snapshots, authoritative ledger and session orchestration |
| future `internal/web` | `net/http` authenticated loopback UI and `embed` assets |
| future `internal/execution`, dedicated root supervisor entry point | noninteractive execution; privilege boundary depends on separate root probe |

These names are seams, not precreated empty packages. Do not put transport,
credentials, or command execution in `contracts`. Keep the concurrent root probe
in its own subdirectory; this foundation makes no claims that it has validated
pkexec/systemd behavior.

## v1 protocol rules

* One bounded JSON value per invocation, then EOF; stdout contains JSON only,
  diagnostics on stderr. Enforce read/write deadlines as well as byte limits.
* `ParseRequest`: at most 512 KiB (including worst-case JSON escaping of a
  64 KiB script and 4 KiB cwd); `ParseReply`: at most 8 MiB (base64 expansion of
  4 MiB capture plus bounded execution metadata). Script at most 64 KiB.
* Exact snake_case, except the closed environment's `PATH`, `HOME`, `LANG`.
  Reject unknown/missing/case-aliased/duplicate fields (including escaped aliases),
  null anywhere, trailing values, malformed Unicode, fractional/exponent integers,
  integer overflow, unknown versions and invalid semantic values. Optional means
  **absent**, not null. Empty output is `""`; empty redaction list is `[]`.
* `submit` requires proposal; `check`/`cancel` forbid proposal. Adapter defaults
  missing MCP timeout to 120 seconds **before** encoding this complete contract.
  v1 remote requests always include timeout. Maximum is 1800 seconds.
* IDs/nonces are 32 CSPRNG bytes encoded as 64 lowercase hex characters. Generate
  host IDs using `crypto/rand`, never from guest paths or timestamps. Request IDs
  are untrusted guest choices: `(session_id, request_id)` retries with identical
  contents return the existing result; changed contents must yield `conflict`,
  not overwrite a snapshot. Replaying an expired session must not enqueue work.
* Host revision starts at 1; every review edit increments it. It is **not** a guest
  revision. Approval binds session, host snapshot ID, revision, epoch, spec digest,
  expiry and one-use nonce. No approval or nonce is sent to the guest.
* `Freeze` and `Digest` validate shape/policy only. They neither approve nor resolve
  the supplied shell, authenticate a user, check filesystem permissions, or verify
  reviewer liveness. Only the host can freeze trusted review input. Resolve bash
  to its `/nix/store/.../bin/bash` path on the host before freeze. The guest cannot
  choose a shell or environment. Runtime must use the frozen path, not PATH lookup.
* Digest algorithm is the documented Go struct encoding in `types.go`, pinned by
  a golden test; do not reserialize with another language's defaults. Shell argv,
  stdin policy, no PTY, and clean environment are immutable v1 policy. Protocol
  changes that alter execution or canonicalization require a new version.
* `Artifact.ExecutionDigest` identifies the original spec. When metadata is not
  redacted it must match `Executed.Digest()`. Marked redactions intentionally make
  that check unavailable: this is **not** cryptographic proof of execution. v1
  supports only script/cwd/stdout/stderr redaction; a replacement script must be
  nonempty and a replacement cwd must remain an absolute clean path (for example
  `/[REDACTED]`). This keeps the typed artifact structurally valid. A more flexible
  redaction format requires a new version; never silently omit metadata.

A parser's validation error may contain guest-controlled field names: keep it on
host diagnostics, escape it for a browser, and never return it as a guest process
error. Replies expose only fixed coarse error codes. No raw stdout/stderr, exit
info, reviewed edits or executed metadata before explicit release. Queue backpressure
must reject (`queue_full`); do not accept silently and drop later.

## Dependencies and MCP decision

Go replaces the earlier Python direction. Foundation has **zero external Go
modules**, no frontend toolchain, no framework and no SQLite driver. Use
`net/http`, `encoding/json`, `os/exec`, `embed`, `crypto/rand`, and Linux flock
(`syscall.Flock`) when those boundaries are implemented. [The file-store
proposal](docs/storage-and-workers.md) deliberately trades SQL transactions for a
single-writer, whole-record replacement protocol; it is not a completed store.

**MCP SDK assessed, not imported:** the official
[`modelcontextprotocol/go-sdk`](https://github.com/modelcontextprotocol/go-sdk)
provides stdio transport, initialization/version negotiation, tool schemas,
JSON-RPC handling and cancellation. These are real benefits over maintaining a
home-grown MCP stack. However, the upstream `main`
[README](https://github.com/modelcontextprotocol/go-sdk/blob/main/README.md) and
[go.mod](https://github.com/modelcontextprotocol/go-sdk/blob/main/go.mod) inspected
for this foundation show Go 1.25 and a substantially wider module graph, including
JSON Schema, URI templates, JWT, OAuth, segmentio encoding/asm, and x/* packages.
That is a module-level inventory, **not a claim all are linked into a stdio binary**.
There is no MCP implementation here to justify that surface today.

Decision: **defer adding an SDK, do not build an ad-hoc transport in its place**.
The guest worker should assess a pinned official stable release using
`go list -deps` and its actual stdio imports, compatibility with Pi native MCP,
security fixes and closure size. Prefer the official SDK if avoiding protocol
maintenance outweighs the measured dependencies; explicitly document that decision
and version/toolchain increase before importing it. No third-party SDK is selected.
MCP tool parameters map into these contracts; SSH request JSON is not MCP JSON-RPC.

## Validation

```sh
cd modules/flakes/host-command-review
go test ./...
go test -race ./...
go vet ./...
go test ./contracts -run '^$' -fuzz FuzzRequest -fuzztime 5s
```

Go 1.23+ is the source baseline (tested with available Go 1.26.10); there is no
`go.sum` because there are no external modules. With no Go on PATH:
`nix shell nixpkgs#go --command ...`. No Nix files or host inputs are changed.
These tests cover contracts, not real SSH isolation, browser defenses, atomic
persistence, execution, polkit or systemd. Later workers must add those tests.
