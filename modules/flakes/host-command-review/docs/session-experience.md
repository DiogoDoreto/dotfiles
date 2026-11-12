# Application, review sessions, and browser access

Implemented against baseline `9c572e5e864921b39c5bb29f1570329375746471`.
This is implementation/test evidence, not desktop deployment acceptance.

## Operating the application

Launch **Host Command Reviewer** from Plasma's application launcher (or run
`agent-host-review` locally). No terminal is required; startup failures produce a
fixed, secret-free KDialog error. This is explicit launch only, not autostart.
Local active dog/Plasma checks, independently enrolled SSH pins and immutable
execution paths remain mandatory. The application initially starts a session.

Repeated launcher activation opens the *same application and review session*,
including its idle screen after End. A new one-use fragment URL is issued through
a private same-UID Unix socket, not HTTP. Issuing the URL preserves existing
browser credentials and liveness; only its successful browser exchange revokes
old access. Lost socket responses or browser-opener failures therefore cannot
lock out the current tab. Activation during a lifecycle operation fails promptly
as busy and can be retried, rather than waiting past the socket deadline. Exchange
does not extend or replace the review session. Never copy the fragment URL to the
agent. The launcher never logs it.

The UI displays the routing code and fixed expiration, browser approval lease,
and agent transport route separately. The copyable instructions are plain text;
copy manually if the clipboard API is unavailable. The code identifies a route,
not authority. Requests still require separate exact command approval and output
release. Transport connected means the independently renewing route is active,
not that every queue publication succeeded or the guest is trustworthy.

* **End** blocks admission/approval, invalidates bindings, cancels and joins work
  (including root authentication), publishes final permitted ledger state while
  renewal remains available, closes/joins the route, then deletes raw captures.
  The application remains idle. Publication/close are bounded best effort over
  SSH: network loss may delay the guest's final view until its lease expires.
* **Start new session** allocates a new long identity and short code. No proposals,
  offers, execution context, browser heartbeat, or UI drafts transfer.
* **Restart** uses that exact End path before Start. Confirmation warns about
  running work, irreversible side effects and deletion of unreleased output.
* Cleanup/reconciliation failure latches **cleanup_failed** and blocks Start or
  Restart in that application. Quit and investigate the private store offline;
  do not remove lock inodes or treat malformed data as an empty store.
* **Quit** is separate: it completes the same end/reap path before exiting.
  Signals, inactive desktop and fatal monitor/server failures also end/reap.
* Session expiry/lost renewal ends that session, leaving an idle application.
  Review sessions retain the four-hour monotonic/suspend-inclusive cap.

## Deliberate browser-storage policy change

Previously refresh lost RAM-only credentials. The UI now stores **only** auth,
CSRF and their absolute expiration in tab-scoped `sessionStorage` (one JSON entry,
512-byte read cap). No host command, output, approval binding, draft or routing
code is persisted. No cookies or `localStorage` are used. Fragment bootstrap is
still one-use, expires after two minutes and is removed from history immediately.
Malformed, oversized, expired or server-rejected credentials are cleared. Storage
unavailability falls back to RAM: use the launcher to reopen after refresh.

This deliberately allows same-origin script/browser-session restoration to recover
credentials. An extension or same-user process controlling the browser remains
outside the threat boundary. Session storage is not a singleton guarantee:
duplicated/restored tabs may inherit it. Each page load generates a fresh RAM-only
256-bit tab identity and explicitly claims browser approval ownership. **Newest
loaded tab wins**; claim revokes the prior lease/epoch, and other tabs' authenticated
reads, heartbeats and mutations fail closed. Reload a tab to reclaim deliberately.
No tab silently retries a failed mutation. A new claim always needs a fresh
heartbeat before run. Refresh never changes the review session's expiration.

Browser authentication expires four hours after issuance regardless of session
Start/Restart or heartbeat. Once it expires, reopen via the desktop launcher for
a fresh fragment. Only explicit protected local activation can issue new browser
authentication; there is no unauthenticated HTTP issuance endpoint. Reopening may
interrupt another tab's review but cannot restore a spent nonce or execute work.
Strict Host/Origin/CSRF/media-type/body parsing and CSP protections remain in force.

## Routing code contract and retained tombstones

A code is eight independent crypto-random symbols from
`23456789ABCDEFGHJKLMNPQRSTUVWXYZ` (40 bits), displayed `ABCD-EFGH`.
Only the MCP `session_id` input accepts case-insensitive or unhyphenated forms.
Exactly eight ASCII symbols are required: no prefixes, whitespace, ambiguous I/O,
Unicode normalization or fuzzy resolution. Request IDs and all security-sensitive
identities remain 32-byte lowercase hex. Internal transport, snapshots, approvals,
releases and replies retain their long identities. A successful MCP call always
returns that same canonical long `session_id`, in identical structured/text replies;
subsequent calls may use either the supplied code or canonical long identity.
An unknown code returns fixed `no_reviewer`, never the current session.

The host collision-checks allocation through guest announcement, bounded to 16
attempts. Alias collision includes closed routes. **Small alias tombstones are
retained permanently in the guest database** while proposal/release history still
expires after seven days. This is a deliberate storage-policy adjustment: recycling
short codes after seven days could redirect a stale instruction to unrelated work.
Permanent reservations consume the existing 900-MiB database budget; exhaustion
fails explicitly rather than evicting reservations. Legacy sessions without an
alias keep their existing retention. Database reset/loss or malicious guest root
can erase reservations, just as it can fabricate all guest routing; no guest
assertion authorizes execution. The host always creates a fresh long identity.

## Implementation seams and validation boundary

`reviewer.Application` owns session transitions under one mutex; it replaces the
entire controller/adapter/worker bundle. `Adapter.Quiesce` and
`Controller.BeginEnd` separate authority revocation/reaping from final cleanup.
Only the worker's existing allowlisted publication path sends data to the guest.
The launcher owns the private control socket and application lifetime; `web.Server`
owns browser authentication/claims, not session lifetime. There is no native
wrapper, web framework, daemon installation or guest execution change.

Regression tests cover root-auth cancellation/join on Restart, stale approvals and
aliases, idle expiry/new sessions, held-output deletion, injected cleanup failure,
exact/colliding closed aliases, browser reload/duplicate claim/liveness, fixed auth
cap, local bootstrap rotation, bounded repeated local activation, and DOM storage/
session-draft behavior. Existing execution/controller tests cover consumed nonce,
root/dog cancellation, held release, suspend and clock guards. Full Go/race/vet,
nested probe and JS checks and Nix validation are recorded in the implementation
report. Real Plasma launcher/KDialog, browser session restore/multi-tab behavior,
clipboard, suspend and actual polkit/systemd cancellation remain manual acceptance
gates. No activation or deployment was performed.
