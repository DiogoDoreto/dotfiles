---
description: returns a git commit title for the currently staged changes
model: github-copilot/gpt-5-mini
agent: title
---

Your task is to write a git commit title for the diff changes below.
Write ONLY ONE single line, nothing more.

Focus on what was added and what was removed.
Be consise.

This is the diff of the changes performed:

<git diff>
!`git diff --cached --unified=10 -- . ':!package-lock.json' ':!*.lock'`
</git diff>

Now reply with ONLY the commit title line:
