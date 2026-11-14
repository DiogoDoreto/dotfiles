# Issue tracker: Local Markdown

Issues and specs for this repo live as Markdown files in `.private/`.

## Conventions

- One feature per directory: `.private/<feature-slug>/`
- The spec is `.private/<feature-slug>/spec.md`.
- Implementation issues are one file per ticket at `.private/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`, never a single combined tickets file.
- Refer to a ticket by feature and filename; bare numbers are ambiguous across features.
- Category and triage state use `Category:` and `Status:` fields; see `docs/agents/triage-labels.md` for the configured role strings. Accept the Markdown forms `**Category:**` and `**Status:**` too, updating whichever form is already present.
- Comments and conversation history append under `## Comments`.
- Implementation claims use `Assignee:`. Completed work closes with `Status: resolved`; rejected or already-implemented requests close with `Status: wontfix` and an explanation.
- Dependencies use `Blocked by: NN, NN` within the feature, or explicit paths across features; the Markdown form `**Blocked by:**` is accepted too. A ticket is unblocked when every blocker is closed. Check rejected blockers for invalidated assumptions before starting dependent work.
- The implementation frontier is the unresolved, unblocked, unassigned tickets, in number order.
- Resolve the spec with `Status: resolved` once all its tickets are closed and verification is complete.
- External PRs are not a triage request surface for this local tracker.

## When a skill says "publish to the issue tracker"

Create a new file under `.private/<feature-slug>/` (creating the directory if needed).

## When a skill says "fetch the relevant ticket"

Read the referenced file.

## Wayfinding operations

Used by `/wayfinder`. The map is a file with one child file per decision ticket; these do not receive implementation triage roles.

- **Map**: `.private/<effort>/map.md`, the Notes / Decisions-so-far / Fog body.
- **Child ticket**: `.private/<effort>/issues/NN-<slug>.md`, numbered from `01`, with the question in the body. A `Type:` line records the type (`research`/`prototype`/`grilling`/`task`); a `Status:` line records `claimed`/`resolved`.
- **Blocking**: a `Blocked by: NN, NN` line near the top. A decision ticket is unblocked when every file it lists is `resolved`.
- **Frontier**: scan `.private/<effort>/issues/` for open, unblocked, unclaimed files; first by number wins.
- **Claim**: set `Status: claimed`, the session's first write.
- **Resolve**: append an `## Answer`, set `Status: resolved`, and append a context pointer (gist + relative link) to the map's Decisions-so-far.
