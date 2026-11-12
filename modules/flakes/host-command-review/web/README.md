# lapdog loopback reviewer

The authenticated view exposes the immutable snapshot's original `reason`, not a
synthetic request label. The UI labels it agent-provided/untrusted and read-only,
above editable execution fields, through all states. Plain `textContent`,
`white-space: pre-wrap` and `overflow-wrap: anywhere` preserve lines without HTML
interpretation or unbounded horizontal overflow. Host edits retain that reason;
reason-only changes invalidate substantive UI review and host review offers
without changing execution hashing. Copyable instructions use `propose_command`
and returned IDs, never manual ID generation. See [interface/recovery and strict
old-data compatibility](../docs/command-interface.md).

Host-private stdlib `net/http` + `embed` implementation. No frontend dependencies,
execution or persistent storage inside this package. `../reviewer` now supplies
the authoritative adapter and `cmd/agent-host-review` supplies the manual entrypoint;
see [operations](../docs/operations.md) for Nix wiring and pending live gates.
This package must **not** be exposed in the Agent VM or reverse-proxied.
The foundation root README predates this work; this package implements its web seam.
Normative shared contracts remain in [`../contracts`](../contracts),
[`state-machine.md`](../docs/state-machine.md), and
[`storage-and-workers.md`](../docs/storage-and-workers.md).

## Design sandbox (no host access)

From `modules/flakes/host-command-review`:

```sh
node web/dev.cjs
# Open http://127.0.0.1:4173/#demo
# Optional port: node web/dev.cjs 4174
```

This dependency-free, loopback-only Node server serves the **real bundled UI**
against an in-memory fake API. A conspicuous sandbox banner is injected only by
this runner; production has no demo switch, mock routes or mock imports. Nothing
executes, no real credentials are used, and restarting resets the fixtures.
Do not expose this development server to a network or use it as a real reviewer.
It deliberately does not implement the authoritative backend's security guards.

Fixtures include ordinary/root commands, held output with a private path, an
uncertain outcome and completed work. Editing updates the revision; running
simulates starting → running → held output. Reject, stop, release and end-session
mutate only memory. Refresh is also supported in production through bounded tab credentials; the sandbox does not enforce production ownership or authority.
Change fixtures in `dev.cjs` to explore long reasons, errors or larger output.

For repeatable real-browser flow checks, start a **fresh** sandbox, then:

```sh
playwright-cli -s=review open http://127.0.0.1:4173/#demo
playwright-cli -s=review run-code --filename=web/browser-check.js
# If the CLI returns early at the run confirmation, the script continues.
playwright-cli -s=review eval 'window.__reviewCheck'
playwright-cli -s=review close
```

The script throws on failures and sets `window.__reviewCheck` to PASS only after
edit/run/held/redact/preview/release, distinct root action, mobile overflow and
withhold checks complete. Playwright is optional and is not a production or Go
test dependency. See [design notes](DESIGN.md) for choices and further opportunities.

## Integration API

```go
reviewer, secretURL, err := web.New(hostAdapter) // binds 127.0.0.1:0 itself
// Handle err. Open secretURL locally without logging it or exporting it.
// Supervise Serve; on exit/session end call Close and end the host session.
err = reviewer.Serve()
```

`New(Backend) (*Server, string, error)` creates fresh independent 256-bit bootstrap,
auth and CSRF capabilities using crypto/rand. The bootstrap URL contains the
one-use token in a **fragment**, never a query. Bootstrap expires two minutes after
construction or protected local reissuance; successful exchange irreversibly spends it.
Local reissuance leaves existing browser credentials valid until exchange succeeds,
so a failed opener or lost control-socket response cannot revoke the current tab. The UI immediately
removes the fragment from history before any fetch. Credentials now survive refresh in bounded tab-scoped `sessionStorage` (auth,
CSRF, absolute expiry only; no host text or bindings). A fresh RAM-only tab claim
revokes prior browser liveness; newest loaded tab owns approval. See
[session experience](../docs/session-experience.md) for the deliberate policy
change, duplicate-tab behavior and protected local bootstrap reissuance. A malicious local
browser extension or host user with process access is outside this boundary.

`Server.Serve()` serves its private loopback listener with bounded HTTP timeouts
and headers. Do not replace its Handler/listener, put it behind a proxy, or log
request bodies, headers or the secret URL. `Server.Close()` closes the listener,
revokes the browser lease, and invalidates the web session. It does **not** itself
stop a process tree: the host owns that duty and must supervise server failures.

### Backend interface (three methods)

* `View(context.Context) (View, error)` returns bounded **host-private** review
  records, complete frozen specs and current host-minted `ApprovalBinding`s.
  `RequestView.Reason` is untrusted guest text. `Held` is the original host-private
  capture in `contracts.Artifact` shape; it is **not** a released artifact or a
  `contracts.Reply`. The adapter must prepare the candidate artifact identity and
  explicit empty streams/redaction list. Never route View over guest RPC.
* `Reviewer(context.Context, time.Time) error` updates the authenticated browser
  deadline. Zero revokes it. Apply and execution consumption must recheck that
  deadline under the same authority lock as nonce consumption. No TCP keepalive
  inference. An expired lease blocks approval even if no revocation callback ran.
* `Apply(context.Context, Operation) error` performs authoritative host operations.
  The web layer validates bounded closed JSON and contract shapes, then forwards
  the **unchanged binding**. It does not validate current ledger state or execute.
  Errors return fixed HTTP status text to the local browser, not backend diagnostic
  strings; retain useful diagnostics in host-only logging inside the adapter.

`Operation` always requires `action` and the complete `binding`. `proposal` and
`artifact` are absent unless that action requires them (null is rejected):

| action | payload | adapter requirement |
| --- | --- | --- |
| `edit` | proposal | check exact current binding; freeze new trusted spec, increment revision, rotate nonce; never launch |
| `run` | none | require frozen user dog; exact current binding and all atomic approval/consume guards |
| `run_root` | none | require frozen user root; same guards; bounded per-command KDE/pkexec authentication in backend, no web passwords |
| `reject` | none | host state-machine guard; no execution |
| `cancel` | none | serialize before-start cancellation vs consumption; request supervisor tree stop if already consumed; do not claim reversal |
| `release` | artifact | exact held result/binding; independently validate allowed redactions and immutable original identity/metadata; publish separate immutable artifact, then mark released |
| `end`, `start`, `restart`, `quit` | all-zero binding | application lifecycle; end/restart cancel and reap, publish and close route, delete raw before replacement; browser auth has an independent fixed cap |

For `release`, the backend **must not trust the browser's artifact identity,
redaction markers or executed history**. Compare with the held original. Only
`executed.script`, `executed.cwd`, `stdout`, `stderr` may differ, with explicit
markers; retain original digest/raw record. All other fields must match the
original. Contract validation alone cannot establish these properties. If a field
such as environment, exit info, user or shell is sensitive, withhold the whole
artifact; v1 cannot redact it. Guest polling may return only coarse status until
an immutable release is durably published. Never serialize `RequestView` to guests.

### Authority and lifetime still owned by the host adapter

Enforce all shared state-machine guards under the host ledger lock: current
session, snapshot, revision, epoch, recomputed digest, fresh unspent nonce and its
expiry, durable intent before prompting/launch, and one starting/running command
**including root authentication**. Block edits while starting/running. Pass web
calls through the same controller as other host lifecycle events. Respect request
contexts and bound backend calls; do not wait for root authentication in Apply.
Reserve durable intent then launch asynchronously, returning a starting view.
Host resume must rotate epochs and invalidate outstanding approvals. Host session
expiry, shutdown, reviewer death and process-tree cleanup must be enforced
independently of this HTTP server and browser JavaScript. Web construction time
is not a substitute for the host's session creation deadline: use the earlier
host deadline. Host liveness does not imply guest lease/SSH liveness.

The browser authentication hard cap is four hours from construction or successful exchange of an explicitly reissued local bootstrap, not extended by review-session restart. Authenticated POST
heartbeats renew a **10-second** browser lease (clamped to that cap), every three
seconds while the document is visible. Authenticated GET polling does not renew
it or change backend state. Closing, hiding, suspending, disconnecting or killing
a browser stops renewal: new approvals fail closed no later than lease expiry.
This is bounded disconnect detection, **not instantaneous physical-disconnect
notification**. The UI locally blocks run when its lease expires; the backend
must independently recheck at approval and consumption, and already consumed work
may finish. Timer throttling can intentionally fail closed. An authenticated
reconnect can heartbeat again but cannot revive spent/stale bindings.

## HTTP security and endpoints

Only exact bound `127.0.0.1:port` Host accepted (no localhost aliases, DNS names,
proxy headers or absolute-form requests). Nonempty Origins must match the exact
origin; every POST including bootstrap requires it. Foreign Fetch Metadata is
rejected. No CORS, redirects, query parameters, mutation GETs or arbitrary files.
All API POSTs require exact `application/json` without parameters. After bootstrap,
all API reads and writes require `Authorization: Bearer <auth>`; writes also need
`X-CSRF-Token`. Duplicate auth/Origin/CSRF headers fail closed.

* `POST /api/bootstrap`: `{ "token": "..." }` -> `{ "auth": "...", "csrf": "...", "expires_unix": 123 }`.
* `GET /api/view`: authenticated host-private View, no lease renewal.
* `POST /api/claim`: `{ "tab": "<64 hex>" }` claims this page's browser approval ownership, revoking the old lease. Authenticated APIs other than bootstrap/claim require the exact `X-Review-Tab` header (duplicates rejected).
* `POST /api/heartbeat`: `{}` -> `{ "lease_seconds": 10 }`.
* `POST /api/operation`: Operation -> `{}` on success. Backend refusal is 409;
  no automatic retry on failures (a network failure can follow committed intent).

Strict parser rejects unknown, missing, duplicate, case-aliased or escaped-duplicate
keys, null at every level, trailing values, invalid UTF-8/lone escaped surrogates,
numeric coercions/overflow and excessive nesting. Bodies are capped at 1 KiB for
bootstrap, 128 bytes for heartbeat, 9 MiB for operations (4 MiB combined output
base64 plus bounded metadata); field bounds use shared contracts. The backend
must additionally bound queue lengths, View data and all persistence/capture.

Every response, including errors/assets, is no-store/nosniff/no-referrer and denies
framing. CSP permits only same-origin bundled scripts/styles/connect and no
inline script/style, unsafe eval, remote resources, frames, forms or base URLs.
JSON uses encoding/json HTML escaping. All UI strings use textContent/value, never
HTML/Markdown/ANSI interpretation. HTTP loopback header auth deliberately avoids
HTTP cookies and their Secure-over-HTTP policy ambiguities.

## UI/output policy

Prominent lapdog, arbitrary authority and guest-writable/live-filesystem warnings.
Request reasons are untrusted and serve as request titles. Actionable requests
are separated from collapsed recent activity. Frozen execution policy is shown
as labeled fields in a disclosure; opaque IDs, hashes and approval capabilities
remain internal, not user-facing descriptors. Command text, cwd, user and timeout
can be edited (save then review the new host spec before run). Defaults belong to host admission:
120 seconds, maximum 1800; one command, four-hour session maximum. Distinct dog
and root run actions; no prompt or password in browser. Starting/running shows
read-only metadata plus explicit stop request. Polling never silently overwrites
unsaved review. Nonce/expiry-only offer renewal updates the host-provided binding
in place without discarding command drafts, redactions, or the unsaved-command run
interlock. Selecting the same unchanged request also preserves drafts. Expired
bindings cannot be submitted; a renewed release offer requires a fresh complete
preview and confirmation. Revision, epoch, digest, phase, spec or held artifact
changes disable stale forms until explicit reselection and fresh review.

**All output is held initially; no skip-output-review option exists.** Successful
execution/polling never releases. Held results expose host-private stdout/stderr,
executed metadata and exit/truncation for review; original identity/digest are
preserved internally without displaying meaningless hashes. Editable text
redactions are explicitly marked, and a whole-export preview is required before
the release button enables, followed by confirmation. Editing invalidates preview.
Withhold sends nothing and retains held state; end-session cleanup is backend duty.
Output display is UTF-8: untouched base64 streams preserve exact original bytes;
editing replaces the whole stream with UTF-8, explicitly marked as a redaction.
The complete preview shows decoded streams, byte counts, redaction markers and
all execution/exit/capture metadata as labeled content, not raw JSON or base64.
Transport IDs, revision and digest are retained unchanged in the release payload.
Invalid UTF-8 is visibly replacement-decoded as documented beside the editors;
untouched byte streams still retain their exact originals. A host-adapter refusal always wins over UI affordances.

## Validation

From the parent module:

```sh
go test ./web
go test -race ./web
go vet ./web
node --check web/assets/app.js
node --test web/assets_test.cjs
```

Tests use a mock authoritative backend, httptest and a real ephemeral loopback
HTTP listener. They cover malicious Host/Origin/CSRF, duplicate headers, bootstrap
expiry/replay, read auth, bounds/strict JSON, Unicode, stale binding forwarding,
nonce replay, lease expiry, GET nonmutation, root/dog action distinction, explicit
release validation, host-error isolation, safe text rendering and security headers.
Node's built-in test runner supplies a minimal DOM harness for fragment removal,
plain text, edit/run interlock, root starting readonly state and release preview/
redaction/withhold behavior. It is **not a real browser** or an extra dependency.

The mock UI was exercised in Chromium using `browser-check.js`, including desktop
and 390px mobile layouts and ordinary edit/run/disclosure flows. This does **not**
verify production bootstrap/auth, actual browser-origin/Fetch Metadata behavior,
history/back-button, multi-tab/visibility/throttling/disconnect, file/extension
attacks or KDE prompts. Before deployment test these against the real reviewer,
along with adapter tests for durable nonce/cancel races, suspend/resume, root auth,
supervisor cleanup, publication and raw-vs-guest isolation. No deployment was performed.
