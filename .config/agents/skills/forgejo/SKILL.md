---
name: forgejo
description: Work with Forgejo repositories using the fj CLI. Use when a user asks to open or inspect a Forgejo pull request, manage Forgejo issues, check PR/CI status, or otherwise interact with a Forgejo-hosted repository, including when the remote is an SSH URL rather than a github.com URL.
---

# Forgejo workflows

Use `fj` for Forgejo operations and `git` for local branches, commits, and pushes. Run commands from the repository directory (or pass `fj -C <directory>`). Check `fj <area> <command> --help` before using an unfamiliar operation: flags differ between subcommands and versions.

## Identify the repository and account

```sh
git status --short --branch
git remote -v
fj repo view
fj whoami
```

An SSH remote such as `ssh://forgejo@host:2222/owner/repo.git` can still be a Forgejo repository. Do not assume `gh` works just because the task involves a PR. If authentication is missing, inspect `fj auth list` and `fj auth login --help`; ask the user for credentials only when needed, and never expose tokens in output. If repository inference is ambiguous, check `fj repo view --help` and use its `-R <remote>` or `-H <host>` options as appropriate.

## Open a pull request

1. Confirm the target branch and inspect the entire proposed change before committing or opening the PR:

   ```sh
   git status --short --branch
   git diff
   git diff --check
   git log --oneline -10
   git remote -v
   ```

2. Run the repository's relevant checks. If changes are uncommitted and the user requested a PR, create a descriptive branch, stage only intended files, and commit. Never fold unrelated work into the PR.
3. Push the branch, then inspect what the PR will contain (including every commit):

   ```sh
   git push -u origin <branch>
   git status --short --branch
   git log --oneline origin/<base>..HEAD
   git diff --check origin/<base>...HEAD
   git diff origin/<base>...HEAD
   ```

   Substitute the actual remote and base branch; fetch the base if the remote-tracking ref is stale. Confirm the remote tracking branch and target repository before publishing.
4. Create the PR with explicit base, head, title, and body to avoid opening an interactive editor:

   ```sh
   fj pr create --base <base> --head <branch> --body '## Summary
   - Describe the change.

   ## Verification
   - Describe checks run.' 'Descriptive PR title'
   fj pr view <number>
   ```

   The command prints the PR URL. Return that URL to the user. Use `fj pr status <number>` to check mergeability/CI when relevant; `--wait` waits for checks to finish. Prefix a title with `WIP: ` only when a draft PR was requested.

## Inspect and manage existing work

```sh
fj pr search
fj pr view <number>
fj pr view <number> diff
fj pr view <number> commits
fj pr status <number>
fj issue search
fj issue view <number>
```

For comments, edits, labels, reviews, merges, issue creation, or Actions, inspect the corresponding `fj pr ... --help`, `fj issue ... --help`, or `fj actions ... --help` first. For example, `fj issue create --body 'Details' 'Issue title'` creates an issue without launching an editor. Prefer explicit bodies or `--body-file` in non-interactive sessions. Verify the resulting PR or issue and share its URL or identifier.
