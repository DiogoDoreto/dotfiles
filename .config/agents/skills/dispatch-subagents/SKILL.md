---
name: dispatch-subagents
description: >
  How to dispatch long-running LLM work as a background subprocess using the `pi` CLI inside a tmux session.
  Use this skill whenever the user wants to run a task in the background, delegate work to another agent,
  parallelize LLM tasks, or kick off something that may take a long time without blocking the current session.
  Also trigger when the user says things like "run this in the background", "do this in parallel",
  "spin up a subagent", or "let it run and I'll check later".
---

# Dispatch Subagent

Run LLM tasks as background subagents using `pi -p` (non-interactive/print mode) inside a dedicated tmux session. This lets you fire off work that takes minutes or longer, return to your current session immediately, and come back to read results when ready.

## Core Pattern

```bash
# 1. Start the subagent in a named tmux session
tmux new-session -d -s <task-name> -x 220 -y 50 \
  'cd /path/to/project && pi -p "your task prompt here" > /tmp/<task-name>.txt 2>&1; echo "[DONE]" >> /tmp/<task-name>.txt'

# 2. Verify it started
tmux ls

# 3. Check progress (output streams to the file as pi runs)
tail -f /tmp/<task-name>.txt

# 4. Detect completion — session exits when pi -p finishes, or check for sentinel
tmux has-session -t <task-name> 2>/dev/null && echo "still running" || echo "done"
grep -q '\[DONE\]' /tmp/<task-name>.txt && echo "done"

# 5. Read the results
cat /tmp/<task-name>.txt
```

The `[DONE]` sentinel at the end of the output file is a reliable signal that the run finished cleanly (vs. still in progress).

## Session Naming and Ownership

Other agents (including subagents spawned by other pi sessions) may have their own tmux sessions running at the same time. You must not interfere with work you didn't start.

**Choose a prefix at the start of your work** based on the current project name — use a short abbreviation of the repository or directory you're working in. If the project is `my-awesome-app`, use `maa` or `myapp`. If it's `dotfiles`, use `dot`. Then name every session you spawn as `<prefix>-<task>`:

```
dot-review
dot-tests
dot-schema
```

This makes your sessions easy to identify as a group, ties them clearly to the project, and makes them impossible to confuse with sessions owned by other agents working in other projects.

**Always manage sessions by their exact name.** Never kill sessions by broad grep patterns — another agent's session names might partially match yours. When listing sessions to check status, filter by your prefix:

```bash
# Good — scoped to your prefix
tmux ls | grep '^dot-'
tmux kill-session -t dot-review   # exact name, surgical

# Bad — could match and kill sessions owned by other agents
tmux ls | grep 'review' | cut -d: -f1 | xargs -I{} tmux kill-session -t {}
tmux kill-server   # never, ever do this
```

If you see sessions in `tmux ls` that don't match your prefix, leave them alone.

## Choosing a Task Name

Pick something short and descriptive. Good names: `<prefix>-refactor`, `<prefix>-audit`, `<prefix>-docs`. This becomes both the tmux session name and the output filename, so keep it filesystem-safe (no spaces).

## Passing Context: Files and Prompts

Use `@file` syntax to include project files (source code, configs, docs) as context. The prompt itself should always be a string passed directly to `pi -p`:

```bash
# Include project files for context, describe the task inline
pi -p "@src/auth.ts @tests/auth.test.ts Identify coverage gaps and suggest new test cases"

# Multi-line prompts work fine with shell quoting
pi -p "Review src/api/ for security issues, focusing on:
- Input validation
- Auth token handling
- SQL query construction"
```

## Checking In Without Interrupting

Since all output is redirected to a file, the output file is the only place to monitor progress — the tmux pane itself will be blank while `pi -p` runs.

```bash
# See the last 30 lines of output so far
tail -30 /tmp/<task-name>.txt

# Follow live output until the DONE sentinel appears
tail -f /tmp/<task-name>.txt | grep -m1 "\[DONE\]"
```

## Parallel Subagents

You can run multiple independent tasks simultaneously — each gets its own session and output file. Use your chosen prefix for all of them:

```bash
# prefix derived from project name "dotfiles" -> "dot"
tmux new-session -d -s dot-auth   -x 220 -y 50 'cd ~/project && pi -p "Review auth module" > /tmp/dot-auth.txt 2>&1; echo "[DONE]" >> /tmp/dot-auth.txt'
tmux new-session -d -s dot-api    -x 220 -y 50 'cd ~/project && pi -p "Review API layer"   > /tmp/dot-api.txt  2>&1; echo "[DONE]" >> /tmp/dot-api.txt'
tmux new-session -d -s dot-schema -x 220 -y 50 'cd ~/project && pi -p "Review DB schema"   > /tmp/dot-schema.txt 2>&1; echo "[DONE]" >> /tmp/dot-schema.txt'

# Check only your sessions
tmux ls | grep '^dot-'
```

## Cleanup

Always kill sessions by exact name. Never use broad patterns that could catch sessions you don't own:

```bash
# GOOD: Kill one session by exact name
tmux kill-session -t dot-auth

# BAD: Kill all your sessions
tmux ls | grep '^dot-' | cut -d: -f1 | xargs -I{} tmux kill-session -t {}
```

## Tips

- **Working directory matters**: always `cd` to the right project root in the tmux command so that relative paths in the prompt and `@file` references resolve correctly.
- **Long-running tasks**: `pi -p` can run for many minutes on complex tasks. The tmux session keeps it alive even if your terminal closes.
- **Read-only subagents**: use `--tools read,grep,find,ls` if you want the subagent to analyze but not modify files.
