# Guest queue, fixed SSH exchange and native MCP

Implemented for the **lapdog Agent VM workstream only**. Integration now provides
Nix wiring, native Pi owned-entry merge and a strict-pinned host SSH client;
see [operations](../docs/operations.md). No activation was performed. The guest
itself never executes host commands or authorizes host approvals/releases.

## Trust boundary

`guest.Store` and `transport` are standard-library-only. `guest/mcpstdio` alone
imports the official Go MCP SDK. Host storage must **not** reuse this database or
treat its flock/lease/replies as evidence of host authority. Guest root controls
every byte and can fabricate sessions, proposals, cancellations, acknowledgments
and releases. The SSH authentication boundary is established by host integration,
not by the session ID. Session IDs and human labels are **routing only**. Host
snapshots, deduplication, approval, release allowlists and execution decisions
remain independently host-private.

The queue has no shell, command runner, host filesystem access, approval token,
nonce, environment override or raw-output upload endpoint. `publish` accepts only
a validated `contracts.Reply`; unreleased replies cannot contain artifacts.

## Build and local entry points

```sh
cd modules/flakes/host-command-review
go build ./cmd/host-review-mcp
go build ./cmd/host-review-exchange
```

Both programs default to `$HOME/.local/state/host-review-guest`. A local
`--state-dir /absolute/private/path` flag exists for setup/testing. Both must use
the same directory and UID. It must be mode 0700; existing wrong permissions are
rejected, not silently repaired. Files are 0600, owned by the current UID, regular,
single-link, opened relative to a pinned directory with `O_NOFOLLOW`. Symlink
ancestors are rejected. All cooperating processes take the stable `store.lock`;
never unlink/replace that inode. Local Linux filesystems only.

There is one bounded `state.json` database, not per-ID paths, and no host-store
implementation dependency. Whole-state replacement provides a single transaction
for session/record changes: exclusive flock, exact bounded parsing and invariant
checks, fresh exclusive `state.tmp`, file fsync, renameat, directory fsync. Crash
scratch files are unlinked under the lock without following/opening them. A failed
transaction is an error, not an accepted proposal. A failure after rename may have
committed: retry the same ID/payload. Durability depends on the filesystem honoring
fsync. This is not SQL or an exactly-once execution claim.

Hard bounds (deliberately conservative initial policy):

* **100 nonterminal requests per session**. Only outstanding work consumes
  admission slots; released/rejected/cancelled/uncertain history does not. A full
  session returns `queue_full` without blocking other sessions. A duplicate does
  not consume another slot. At most **16 live routes**; closed/expired session
  tombstones do not consume route slots. Retained history is never silently evicted.
* State JSON has a separate aggregate **900 MiB** byte cap, independent of request
  and route admission counts. Retained releases/proposals/tombstones all count
  against this storage budget. Exhausting it fails the transaction explicitly;
  per-session admission slots are not a reservation of storage capacity.
  Oversized files are rejected by size before reading, with bounded parsing
  as a second check. Atomic replacement needs space for both old and new state
  plus scratch; budget about **1800 MiB**, not just capture size. Whole-state
  parsing/encoding can use substantially more RAM than the encoded file size and
  rewrites the database on each transaction; this is a capacity bound, not a
  throughput or low-memory guarantee.
* Guest session-based retention is seven days, not extended by polling. Closed
  legacy sessions without aliases and their records are removed under the lock after retention.
  Alias-bearing session tombstones remain permanently reserved within the bounded
  database; their records still expire normally. Reset/loss of guest state loses
  these reservations, never host execution guards. Expired
  IDs cannot be announced again while their tombstones exist. The host **must
  always generate fresh random IDs**, including after retention, reset, guest
  reboot or loss of guest state; guest tombstones are not host replay protection.
  Advancing the guest wall clock may shorten guest retention, never host retention.
* A routing lease is **15 seconds**, renewed explicitly every approximately
  **5 seconds** by the host. Only `renew` extends it. `pull`, `publish`, duplicate
  `announce` and MCP activity do not. Max session lifetime **4 hours**. Leases use
  Linux `CLOCK_BOOTTIME` (includes suspend) plus boot ID, not guest wall time.
  Expiry, boot change, backwards boot time or close fails closed; an expired route
  cannot be renewed. Expiry is reconciled on each transaction; no background
  service is required.
* Without a live route, new submit returns `rejected/no_reviewer`. Expired pending
  **unacknowledged** records become `rejected/session_expired`. Already acknowledged
  coarse status/released data is preserved, not invented as a cancellation or
  execution outcome. Host should publish final coarse outcomes before closing.

## Exact fixed executable SSH protocol: transport v1

The intended eventual fixed absolute remote executable is
`/run/current-system/sw/bin/host-review-exchange` **with no arguments**. This path
is a deployment contract, not an installed binary in this change. If packaging
chooses another absolute path, explicitly configure/pin that constant in the host.
Never interpolate scripts, paths, labels or IDs into SSH command strings. Send
JSON on stdin and close it. There is no remote subcommand, shell expression,
filename retrieval, directory enumeration or host listener.

Each invocation reads **one JSON value followed by EOF**, accepts trailing JSON
whitespace only, writes one compact JSON response plus newline and exits. Stdout
is protocol-only; failure/timeout produces a nonzero exit and fixed diagnostic
on stderr, not parser errors containing attacker-controlled strings. Malformed
input produces no response. The executable enforces a hard **10-second process
lifetime**, including input EOF, lock contention, disk I/O and output writes.
Host must independently enforce deadlines and byte limits even if the guest
executable lies, blocks or floods.

`transport.Request`, `Response`, `Item`, `ParseRequest` and `ParseResponse` are the
exported integration API. Version is separately **1**, not an extension of the
foundation tool `contracts.Request`. Exact field names, required fields, absent
rather than null optionals, duplicate/escaped-aliased key rejection, strict
Unicode, integer syntax and semantic checks apply. Host must additionally check
response binding and expected verb semantics; shape validation is not authority.

Request shape:

```json
{"version":1,"operation":"announce","session_id":"<64 lowercase hex>","label":"lapdog"}
```

* `announce`: requires ASCII label `[a-zA-Z0-9 ._-]{1,64}`. Optional canonical
  `alias` is an eight-symbol routing code with a grouping hyphen; collision with
  any retained reservation conflicts. See [session experience](../docs/session-experience.md). Creates a new 15s lease.
  Same ID/label while live is idempotent but does not extend the lease; changing
  label conflicts. Closed/expired retained IDs cannot be reused.
* `renew`, `pull`, `close`: only `version`, `operation`, `session_id` are allowed (no alias).
  `renew` extends a still-live lease, capped at the original four-hour maximum.
  `close` is idempotent for a known route; it expires unacknowledged pending work.
* `publish`: those same three fields plus required `reply: contracts.Reply`.
  Session must match and remain live. It cannot create a request. Publishing
  `pending` acknowledges a durably imported proposal; subsequent publication may
  advance coarse status or explicitly disclose `released` with its artifact.
  No ack verb or guest deletion is required. Terminal publication is immutable:
  identical retries succeed; any changed terminal data conflicts. Coarse
  regressions active->pending or held->active/pending conflict. These checks
  prevent accidental transport regressions, not unauthorized host execution.

Every well-formed response has all four required fields below. Optional `error`
is absent on success. `items` is always an array, never null:

```json
{"version":1,"session_id":"<same ID>","lease_seconds":15,"items":[]}
```

Only `pull` returns items. `lease_seconds` is the remaining lease rounded up,
0..15; close and errors return 0. A pull item is:

```json
{"request":{"version":1,"operation":"submit","session_id":"<same ID>","request_id":"<guest ID>","proposal":{"script":"echo hello","cwd":"/home/dog","user":"dog","timeout_seconds":120}},"cancel_requested":false,"acknowledged":false}
```

Pull returns **all nonterminal records**, including already acknowledged ones,
at most 100 for the requested session, in insertion order. There is no destructive dequeue or cursor.
Host validates and snapshots/deduplicates each `(session_id, request_id)` in its
own store before publication of `pending`. Then it publishes only host-ledger
coarse status and, after explicit host release, the immutable release artifact.
`cancel_requested` is sticky, delivered repeatedly until a terminal publication.
It is only a request; even before acknowledgment, guest cancellation does not
claim the host stopped. Host serializes it against its own execution intent.

Errors are fixed `session_expired`, `not_found`, `conflict`, `queue_full`,
`invalid_request`, with empty items and lease 0. Shape/parser failures instead
exit nonzero. `publish` and `pull` on expired routes fail; terminal releases remain
available to MCP locally after close, subject to guest retention.

Byte caps are exported: request **8 MiB + 64 KiB** (one release), response
**100 * 512 KiB + 64 KiB** (100 proposals). These are independent of foundation
`MaxRequestBytes` (512 KiB) and `MaxArtifactBytes` (8 MiB). Host should construct
and validate requests before encoding, cap encoded bytes, parse responses with
`ParseResponse`, and never trust guest counts/advertised lengths. A guest
response is data, never a path to open or execution instructions.

## Native MCP stdio

`host-review-mcp` uses the official SDK for JSON-RPC, initialization/version
negotiation, tools/list/call, ping, errors and request cancellation. No HTTP, OAuth
endpoint, resource, prompt, model content, extension or Pi glue is started. Only
`submit`, `get`, `cancel` tools are registered. Each tool takes explicit
`session_id` (long identity or short routing code, optional grouping hyphen/case)
and guest-chosen `request_id` (64 lowercase hex). Short codes resolve exactly,
never by prefix; successful replies retain the canonical long session identity. A route is supplied
by future host/Pi integration; the guest does not discover or authenticate hosts.
`submit` additionally requires `proposal` with script, cwd, dog/root user and
optional timeout_seconds, defaulted to **120** before contract validation. There
is no guest shell/environment selection. `get` maps to foundation `check`.

The same pair and exact normalized proposal deduplicate, including default-vs-
explicit timeout; changing any proposal field gives `conflict`. New IDs after a
mutation need new host review. `cancel` sets a sticky request bit and returns the
current coarse reply, not a fabricated stopped outcome. Terminal cancel/get and
submit retries return the same released artifact. MCP JSON-RPC cancellation only
cancels the current RPC; durable queue cancellation requires the `cancel` tool.

Each tool result has exactly one text block, the compact JSON encoding of the
same validated reply in `structuredContent`. There are **no alternative blocks,
resource links, annotations with secrets, raw output or original proposal
fallbacks**. Before release it is coarse status/IDs/error only. After release it
is exactly the released artifact, including the reviewed execution metadata and
base64 output; never the original guest proposal mislabeled as execution.
Tool validation errors are fixed `invalid_request`; local storage/availability
failures are fixed `unavailable`, in identical text/structured error objects.
No arbitrary parser, filesystem or host process diagnostics are returned.

Inbound stdio is newline-delimited JSON, capped **512 KiB + 64 KiB per frame**,
with duplicate-key, Unicode, trailing-value and depth checks ahead of SDK decoding.
EOF in a partial frame is rejected. MCP may be idle indefinitely; once a frame's
first byte arrives it has **10 seconds** to finish. Writes have 10s deadlines;
local queue lock waiting has a 5s context deadline. Pollable pipe descriptors are
required (unsupported regular-file stdio is rejected). The conservative timed
reader performs byte reads to avoid prefetch bypass of frame deadlines. This
prioritizes correctness over inbound throughput; proposals are at most 64 KiB.

**Do not reuse the 8 MiB release bound for MCP responses.** MCP includes both
structured and escaped JSON text copies; the explicit outbound cap is **24 MiB**.
The integration test sends a full 4 MiB arbitrary-byte capture plus a worst-case
escaped 64 KiB script through an actual stdio subprocess and the SDK client,
including its normal 16 MiB inbound default. No output truncation is introduced
by the adapter. The original combined stdout+stderr 4 MiB capture cap still
applies to each artifact. Future native Pi wiring should configure suitable
response limits and preserve unrelated MCP configuration entries.

## SDK decision and measured dependency surface

Pinned **official `github.com/modelcontextprotocol/go-sdk v1.8.0`**, a stable tag
available during implementation; no third-party MCP framework. This intentionally
supersedes the foundation's deferral, raises the module's source baseline from
**Go 1.23 to Go 1.25**, and adds `go.sum`. Tested using Go **1.26.10**.
The SDK's bounded `MaxLineLength` is explicitly configured; our stricter framing
and tool parsers do not replace MCP negotiation/JSON-RPC dispatch. Keeping those
protocol responsibilities upstream is worth the measured dependency surface and
avoids a home-grown MCP implementation. This is dependency assessment, not an
upstream security audit or a claim of zero transitive vulnerability risk.

`go list -deps` for the MCP binary actually imports modules jsonschema-go v0.4.3,
segmentio/encoding v0.5.4, segmentio/asm v1.1.3, uritemplate/v3 v3.0.2,
x/oauth2 v0.35.0, x/sync v0.20.0, x/sys v0.41.0 and x/time v0.15.0, besides the
SDK itself. SDK authentication/HTTP-related source packages are in that import
closure even though no HTTP transport is exposed. The wider `go list -m all`
graph also includes jwt/v5 v5.3.1, go-cmp v0.7.0, x/tools v0.42.0 and
cloud.google.com/go/compute/metadata v0.3.0; these are **not** in the MCP binary's
`go list -deps` import list. Do not confuse the module graph with linked code.

The exchange entry point's `go list -deps` has **zero external packages** because
MCP lives in the separate `guest/mcpstdio` package. Linux amd64 Go 1.26.10 local
`go build -trimpath` outputs were approximately **12 MiB MCP / 4 MiB exchange**
(unstripped); these are binary sizes, not measured Nix closure sizes. Integration subsequently built Nix guest/host packages and tested native Pi 1.5.0
configuration merge/connection; closure sizes have not been measured.

## Validation and remaining work

Run `go test ./...`, `go test -race ./...`, `go vet ./...` and
`go test ./transport -run '^$' -fuzz FuzzEnvelope -fuzztime 5s` from the module.
Tests cover malformed/oversized envelopes, strict MCP framing/arguments, real
stdio initialization and tools, full-size release with identical text/structured
content, hostile text/binary content, no held-data leakage, concurrent OS
processes, flock deadlines, same-ID dedup/conflict, cancellation, renewal,
expiry/boot/clock/session-close behavior, queue bounds, immutable publication,
corrupt files, permissions, symlinks/hardlinks/FIFOs and scratch recovery.
The final transport fuzz run completed approximately 161,000 executions (two
workers, five-second budget). Manual subprocess smoke tests also kept partial
stdin open: both actual binaries exited nonzero with no stdout at approximately
10 seconds, while a separate idle MCP process stayed connected beyond that time.

Not validated: real Pi native-client integration, actual pinned-key SSH transport,
power-loss/fsync fault injection at every boundary, uncooperative guest/root DoS,
network-filesystem semantics, host/browser/execution/lifetime integration,
polkit/systemd, or Nix deployment. 100 outstanding requests per session and sixteen live routes are
an explicit initial backpressure policy, not a production throughput claim.
