# Issue tracker: Local Markdown

Issues and specs for this repo live as Markdown files in `.private/`.

## Conventions

- One feature per directory: `.private/<feature-slug>/`
- The spec is `.private/<feature-slug>/spec.md`
- Implementation issues are separate files at `.private/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`
- Comments and conversation history append under a `## Comments` heading
- Refer to tickets by their feature directory and filename; a bare `NN` is ambiguous across features

## Implementation and triage operations

- Category and triage state use the canonical fields in `docs/agents/triage-labels.md`.
- Newly generated implementation tickets start with `Status: ready-for-agent`.
- Claim an implementation ticket with an `Assignee:` line naming the driving agent/session; leave its triage status intact. Remove the assignment when releasing it.
- Close completed work with `Status: resolved`; close rejected or already-implemented requests with `Status: wontfix`, explaining why under `## Comments`.
- Dependencies use `Blocked by: NN, NN` within the same feature, or explicit paths for cross-feature dependencies. Both `Blocked by:` and the existing Markdown form `**Blocked by:**` are accepted, as are plain and bold `Status:` fields. Update whichever form the file already uses.
- The implementation frontier contains unresolved, unassigned tickets whose blockers are all closed. Check `wontfix` blockers for invalidated assumptions before starting dependent work.
- Resolve a spec by setting its `Status: resolved` after all implementation tickets are closed and verification is complete.
- Triage comments start with the AI disclaimer specified by the `triage` skill, appended under `## Comments`.
- External PRs are not a triage request surface for this local tracker. Creating or reviewing a PR is a separate Git-host operation.
- Rejected-enhancement records live in `.out-of-scope/` as described by the `triage` skill; link them to the local issue paths rather than ambiguous remote issue numbers.

## Publishing and fetching

When a skill says "publish to the issue tracker," create a file under `.private/<feature-slug>/`.

When a skill says "fetch the relevant ticket," read the referenced file.

## Wayfinding operations

- Map: `.private/<effort>/map.md`
- Child ticket: `.private/<effort>/issues/NN-<slug>.md`
- Ticket type is recorded in a `Type:` line
- Ticket state is recorded in a `Status:` line as `claimed` or `resolved`
- Dependencies are recorded as `Blocked by: NN, NN`
- Claim a ticket by setting `Status: claimed`
- Resolve it by adding an `## Answer`, setting `Status: resolved`, and updating the map
