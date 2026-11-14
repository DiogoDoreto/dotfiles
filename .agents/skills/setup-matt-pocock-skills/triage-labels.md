# Triage roles: Local Markdown

Use the canonical role strings without aliases. These are fields in local issue files, not labels to create on a remote Git host.

| Role | Local field/value | Meaning |
| --- | --- | --- |
| `bug` | `Category: bug` | Something is broken |
| `enhancement` | `Category: enhancement` | New feature or improvement |
| `needs-triage` | `Status: needs-triage` | Maintainer needs to evaluate |
| `needs-info` | `Status: needs-info` | Waiting on reporter information |
| `ready-for-agent` | `Status: ready-for-agent` | Fully specified and ready for an agent |
| `ready-for-human` | `Status: ready-for-human` | Needs human implementation |
| `wontfix` | `Status: wontfix` | Rejected or already implemented; closed |

Every triaged implementation issue has one category and one state. Update the existing field rather than adding a second one. A missing category or status is untriaged; conflicting values need maintainer direction.

`Status: resolved` closes completed implementation work. `wontfix` also closes work and satisfies a dependency, but any dependent ticket must be checked for assumptions invalidated by the rejection. Append explanations under `## Comments`.

Wayfinding tickets use `Type:` and `Status: claimed` / `Status: resolved` instead. They are decisions, not implementation issues, and do not receive triage roles. See `docs/agents/issue-tracker.md`.
