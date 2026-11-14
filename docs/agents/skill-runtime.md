# Imported skills: local runtime conventions

These conventions adapt the imported Matt Pocock skills to this repo without tying them to one coding harness.

## Loading skills

When an imported skill says to call the `Skill` tool, load the named skill through the harness's native mechanism. If there is no such tool, read `.agents/skills/<name>/SKILL.md` and follow it. Resolve bundled reference links relative to that skill's directory; resolve project artifacts relative to the working repo unless a skill says otherwise.

## Delegation and isolated checkouts

Use the current harness's supported delegation mechanism and follow its orchestration rules. Requests for background agents, fresh contexts, and isolated worktrees describe the needed execution properties, not permission to bypass the harness with shell-created checkouts. Pass explicit task briefs and absolute artifact paths; if shared files are not accessible to a child, supply their contents or a committed artifact instead.

An imported procedure that calls for a child to run another skill does not override restrictions on nested delegation. Where the harness disallows that nesting, the parent orchestrates the required agents and passes results back. If the requested execution properties are unavailable, explain the limitation before changing the workflow.

## Artifacts and tracker

- Use `.private/tmp/` for temporary files, handoffs, reports, and exploration notes. Pass absolute paths across checkouts.
- Use the local Markdown tracker described in `docs/agents/issue-tracker.md`, including its distinction between implementation status and wayfinding status.
- Use `GLOSSARY.md` and `docs/agents/domain.md` for domain language.
- For visual artifacts, use the current harness's supported rendering tools when available. A path or screenshot saved on disk is not an inline preview. Do not start a server merely to display a self-contained HTML artifact.

## Secrets and Git hosts

Identify the actual Git host before choosing CLI commands. GitHub examples in imported references do not make GitHub this repo's tracker or CI provider. For Forgejo operations, load the available `forgejo` skill.

Keep secrets out of chat, logs, committed artifacts, and agent-visible output. Use the harness's secret-request mechanism when the agent needs a credential. A wizard handed to the user may accept hidden input locally; provider-specific uploads must use the confirmed target repository, not a CLI's unchecked default.
