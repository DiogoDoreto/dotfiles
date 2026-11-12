# Host command MCP interface (unreleased v1)

Only `propose_command`, `get_command_status`, and `request_cancellation` are exposed.
Internal transport operations remain `submit`, `check`, and `cancel`; they are not
additional MCP tools. Routing identifiers are not credentials or execution authority.

## Propose and follow up

Call `propose_command` with flat arguments, for example:

```json
{"session_id":"ABCD-EFGH","reason":"Inspect disk usage before choosing what to clean. Read-only; no deletion.","script":"df -h /","cwd":"/","user":"dog","timeout_seconds":120}
```

`timeout_seconds` is optional (120 by default, range 1..1800). All other fields
are required. The server generates a cryptographically random 32-byte lowercase-hex
`request_id`. Keep the canonical long `session_id` and `request_id` from its reply.
Pass exactly those two fields to `get_command_status` or `request_cancellation`.
The old nested `proposal` and caller-supplied `request_id` are rejected at MCP.
All argument objects are closed: unknown/duplicate/null fields and invalid Unicode
are rejected, not repaired.

A pending reply means local routing/storage, **not host acceptance**, execution,
or permission. The human reviews the command and may edit execution fields before
approving it. Output remains held after execution; a separate explicit human review
and release is required. Before release, replies contain only coarse status, IDs
and fixed errors. Released stdout/stderr are base64 bytes, not necessarily UTF-8;
check redaction markers and executed metadata (which can reflect host edits).
A cancellation reply acknowledges a **request**, not a stopped process or rollback;
side effects may already have happened. Cancellation never discloses held output.
An uncertain execution outcome is not permission to submit a replacement.

## Reason is immutable review context, not executable input

`reason` is required inside the **internal** proposal as well. It must be nonblank
(after Unicode whitespace trimming), valid UTF-8, at most **2048 UTF-8 bytes**,
and without NUL. The original bytes, including newlines, are preserved. JSON Schema
character bounds are only a hint; validation enforces the byte bound.

The reason travels through strict parsers, durable guest records, SSH transport,
and the immutable host snapshot into the authenticated browser view. It is shown
prominently before editable execution fields, labeled agent-provided/untrusted,
read-only through all request states, using textContent, pre-wrap and anywhere
wrapping. It is not a trusted summary of the script: review the actual execution.

Reason participates in original submission equality: the same internal routing pair
with a changed reason conflicts on guest and host. Host editing cannot change it;
the edit must preserve the original reason, snapshot and approval semantics. The
execution spec/digest and release artifact have **no reason field**. Reason changes
invalidate substantive browser review comparison; host review offers also bind the
original reason separately from the execution digest. Normal reason mutation is
impossible without creating a new request. Reason is not automatically disclosed
by releasing output (a script's own output can of course independently contain it).

## Honest retry boundary

Every independent `propose_command` invocation generates a fresh ID even if its
payload equals an earlier proposal. Legitimate repeated commands must remain
possible; payload matching is not an idempotency key. The ID is generated once
before `Store.Handle`; any in-call durable retry uses that same request object.
There is currently no adapter retry loop. Internal queue/SSH retries keep that ID
and the existing exact-payload conflict rules, so they do not create a replacement.

If a response is lost after submission, the caller may not know its ID. **Do not
blindly repeat the proposal or submit a replacement. Reconcile with the human
reviewer first**, including checking host queue/state and possible side effects.
There is no exactly-once submission guarantee across independent MCP calls, nor a
new automatic lookup/recovery tool in this change.

Evidence inspected: pinned Go MCP SDK v1.8.0 `mcp/requests.go`, `protocol.go`
(`CallToolParamsRaw`), `shared.go` (`ServerRequest`, `RequestExtra`) and the local
stdio frame adapter. Call parameters have name, arguments and optional `_meta`;
request extras have HTTP/auth/SSE context, not a supported stable cross-call
idempotency token for stdio. JSON-RPC IDs correlate calls within a connection;
they are not globally stable command identities. Arbitrary `_meta` or progress
identifiers are not specified durable submission identities and are not used as
such. Pi 1.4 MCP documentation explicitly says tool calls are not retried because
the server may already have performed them. No compatible stronger stable identity
was found on which to base transparent recovery without a new protocol contract.

## Intentional strict old-data incompatibility

This is still unreleased v1, not an automatic migration. Existing proposals,
guest queue records and host snapshots lacking `reason` are invalid under the
closed required-field parser/validator. Guest store loading and host record loading
fail closed; records are not silently supplied with invented reasons. This may
block the whole guest store load / host startup or cleanup rather than just omit
one old request. Old execution specs, execution digests and release artifacts do
not gain reason and their encodings are unchanged; their enclosing original
proposal records still need the new field.

Before manually upgrading, finish or deliberately abandon old review sessions,
reconcile any uncertain executions, and preserve any needed host-private results.
If old persistent state exists, stop the applications and have the operator archive
or deliberately remove the old private guest/host stores after reconciling them;
do not modify immutable snapshots to fabricate a reason. This change does not
perform deployment, cleanup, migration or activation. Reload MCP discovery after
updating both components; old tool names are intentionally not aliases.
