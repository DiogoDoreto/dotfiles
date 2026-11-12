# Reviewer session experience implementation plan

Baseline: 9c572e5e864921b39c5bb29f1570329375746471.

## Goal

Make the manually launched host reviewer usable from the desktop application launcher without terminal output, reloadable, and able to start/end/restart review sessions from the web UI. Native wrapping is out of scope. No deployment or activation; real desktop acceptance gates remain pending.

## Requirements

- Separate reviewer application lifetime, review-session lifetime, and browser authentication lifetime. The application remains available after a review session ends or expires.
- Preserve existing execution authority, exact approval guards, browser liveness, durable consumption, cleanup, explicit release, and fixed four-hour review-session cap.
- UI shows session routing code, expiration, browser approval connection and agent transport health distinctly. Provide copyable agent instructions.
- End stops admission/approval, cancels/reaps, publishes final permitted state, closes the route, applies existing raw cleanup, then leaves an idle application with Start new session.
- Restart performs that same end path before creating a fresh session/code; old proposals, bindings, and drafts cannot transfer. Cleanup failure blocks restart. Confirmation warns about running work and unreleased output.
- Quit is separate and ends/reaps before exiting.
- Browser refresh reconnects to the same review session without resetting its deadline. Use bounded tab-scoped sessionStorage credentials, retain one-use fragment bootstrap, clear invalid credentials, and require a fresh heartbeat before approval. Explicitly handle duplicate tabs rather than assuming storage guarantees a singleton. Maintain strict origin/Host/CSRF/content protections; credentials do not establish liveness. Review-session restart must not extend browser authentication indefinitely.
- Short guest-facing routing alias: eight crypto-random unambiguous base32 symbols, displayed with a grouping hyphen, optional hyphen/case accepted at the MCP input. Keep long internal session identities and security-sensitive identifiers unchanged. Collision-check allocation, never ambiguous-prefix match, and never redirect stale codes to current sessions. MCP accepts the short code directly and returns canonical consistent routing information. IDs route only; they never authorize execution. Preserve bounded parsing/queues.
- Desktop launcher activation is idempotent: start/open if absent; reopen existing application without changing its session if present; idle screen if ended. A permission-protected same-user local control socket may issue fresh bootstrap entries; no unauthenticated HTTP credential issuance. Validate local control peers/runtime ownership and bound requests. Startup failures must be visible without a terminal. Explicit user launch only, no autostart.
- Document deliberate browser-storage policy changes and operating instructions. Keep implementation seams small rather than layering ad hoc session checks through unrelated callers.

## Validation

Add regression coverage for refresh during review, restart during root-auth/execution, held output cleanup, rejection of stale aliases/approvals, duplicated-tab policy, repeated launcher activation, expiry and cleanup failure. Run Go tests/race/vet, nested probe tests, JS syntax/DOM tests. Format Nix changes and attempt lapdog flake check and host/guest package builds; record environmental limitations honestly. Do not activate any configuration.
