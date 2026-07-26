# Issue tracker: Local Markdown

Issues and specs for this repo live as Markdown files in `.private/`.

## Conventions

- One feature per directory: `.private/<feature-slug>/`
- The spec is `.private/<feature-slug>/spec.md`
- Implementation issues are separate files at `.private/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`
- Comments and conversation history append under a `## Comments` heading

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
