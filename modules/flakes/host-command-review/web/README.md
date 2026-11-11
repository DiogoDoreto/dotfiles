# lapdog loopback reviewer

Host-private stdlib `net/http` + `embed` implementation. No frontend dependencies,
execution, persistent storage, root helper, guest transport, Nix changes or runnable
entry point. This package must **not** be exposed in the Agent VM or reverse-proxied.
The foundation root README predates this work; this package implements its web seam.
Normative shared contracts remain in [`../contracts`](../contracts),
[`state-machine.md`](../docs/state-machine.md), and
[`storage-and-workers.md`](../docs/storage-and-workers.md).

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
construction; successful exchange irreversibly spends it. The UI immediately
removes the fragment from history before any fetch. Credentials live only in JS
memory (no cookies/localStorage/sessionStorage); reload requires restarting the
reviewer for a fresh session, not replaying the old fragment. A malicious local
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
| `end` | all-zero binding | end host session, cancel/reap work, revoke capabilities and lease; web session is invalidated only on backend success |

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

The HTTP session hard cap is four hours from construction. Authenticated POST
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

* `POST /api/bootstrap`: `{ "token": "..." }` -> `{ "auth": "...", "csrf": "..." }`.
* `GET /api/view`: authenticated host-private View, no lease renewal.
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
Request reasons are untrusted. Full frozen execution policy, revision/digest/epoch/
nonce/expiry are shown; command text, cwd, user and timeout can be edited (save
then review the new host spec before run). Defaults belong to host admission:
120 seconds, maximum 1800; one command, four-hour session maximum. Distinct dog
and root run actions; no prompt or password in browser. Starting/running shows
read-only metadata plus explicit stop request. Polling never silently overwrites
unsaved review; changed host state disables stale forms until reselected.

**All output is held initially; no skip-output-review option exists.** Successful
execution/polling never releases. Held results expose host-private stdout/stderr,
executed metadata, exit/truncation and original digest for review. Editable text
redactions are explicitly marked, and a whole-export preview is required before
the release button enables, followed by confirmation. Editing invalidates preview.
Withhold sends nothing and retains held state; end-session cleanup is backend duty.
Output display is UTF-8: untouched base64 streams preserve exact original bytes;
editing replaces the whole stream with UTF-8, explicitly marked as a redaction.
The complete preview includes wire base64 and all metadata so nothing is silently
omitted. A host-adapter refusal always wins over UI affordances.

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

No browser binary or Playwright/Puppeteer installation was available. Real browser
CSP enforcement, actual browser-origin/Fetch Metadata behavior, history/back-button,
multi-tab/visibility/throttling/disconnect, file/extension attacks and KDE prompts
were **not tested**. Before deployment run these in a local browser harness, along
with real adapter tests for durable nonce/cancel races, suspend/resume, root auth,
supervisor cleanup, publication and raw-vs-guest isolation. No platform preview,
browser/server preview, package installation or deployment was performed.
