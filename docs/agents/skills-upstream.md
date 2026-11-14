# Imported skill provenance

Upstream: https://github.com/mattpocock/skills

Upstream baseline: `5863806dd4e15c63174bee4e7515a4256792ecfa`.

## Local adaptations

- Keep the local Markdown tracker under `.private/`, not upstream's `.scratch/` default. `docs/agents/issue-tracker.md` records implementation claims, completion, dependencies, and the existing separate wayfinding conventions.
- Record canonical triage category/state fields in `docs/agents/triage-labels.md`; no remote labels are created.
- Keep temporary handoffs, reports, exploration notes, and generated wizards under `.private/tmp/`.
- Resolve references to a `Skill` tool through the current harness; without that tool, read the named skill's `SKILL.md`. `docs/agents/skill-runtime.md` is linked from `AGENTS.md` and records tool, delegation, artifact, and secret-handling conventions.
- Adapt `implement-spec` to harness-managed isolated checkouts, avoiding shell-created worktrees or destructive resets.
- Keep the wizard template's GitHub helpers conditional on a confirmed GitHub target. For Forgejo, use the actual provider workflow instead. Generated wizards set `TMPDIR` to the absolute local temporary directory before creating temporary files.
