---
name: forgejo
description: Work with Forgejo repositories using the fj CLI. Use when a user asks to open or inspect a Forgejo pull request, manage Forgejo issues, check PR/CI status, author Forgejo Actions workflows, configure build triggers or static HTML publishing with PR previews, or otherwise interact with a Forgejo-hosted repository, including when the remote is an SSH URL rather than a github.com URL.
---

# Working with Forgejo

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

## Workflows: builds, triggers, and static pages

Store workflows in `.forgejo/workflows/*.yml`. Forgejo supports the familiar Actions YAML and `github.*` compatibility context; do not assume every GitHub action or service is compatible. Inspect existing workflows and available runner labels before choosing dependencies.

### Build triggers and runners

Use this event shape for checks on PR changes, builds after merging, and manual runs:

```yaml
name: Build
on:
  pull_request:
    types: [opened, synchronize, reopened]
  push:
    branches: [main]
  workflow_dispatch:
```

- `pull_request` checks proposed changes; `push` on the default branch checks the merged result. Replace `main` when the repository uses another default branch.
- `workflow_dispatch` enables manual runs. Add typed `inputs` (for example, a `choice` with an explicit `options` list) when users need to select a target. Validate inputs before using them in shell commands; pass expression values through `env` instead of interpolating untrusted values into scripts.
- Use `runs-on: ubuntu-latest` for ordinary container jobs. The local `chungus-nix` label selects a native Nix build runner, not an Ubuntu container; use it only for repositories authorized by the server-side trust policy. Job-level `if` guards are defense in depth, not a substitute for that policy.
- For independent build targets, use `strategy.matrix` and `fail-fast: false` so one failure does not hide other results. For dependent jobs, use `needs`, job outputs, and `$GITHUB_OUTPUT` rather than recomputing a moving branch tip.
- Check out the exact intended revision with `ref: ${{ github.event.pull_request.head.sha || github.sha }}`. Set `persist-credentials: false` unless later steps explicitly need checkout credentials; use `fetch-depth: 0` when history or base comparisons are required.
- For reproducible Nix checks/builds, use `--no-update-lock-file --no-write-lock-file`. Keep lock-file updates in a separate, explicit workflow. Building a system does not authorize deploying or switching it.
- Scope credentials to the publishing step. Do not run untrusted PR code on a privileged native runner or with publishing secrets. For workflows that push generated changes, detect bot-only updates or unchanged output to avoid trigger loops.

### Action resolution and artifacts

On the local Forgejo instance, unqualified `actions/...` references resolve through its local action mirrors. Examples include `actions/checkout`, `actions/setup-python`, `actions/setup-uv`, `actions/cache`, `actions/git-pages`, and `actions/comment-pull-request`. Confirm that the required mirror and version exist; prefer immutable commit pins for security-sensitive jobs. Use a fully qualified action URL when deliberately selecting another host.

The deployed Forgejo artifact service uses the **v3 artifact protocol**, not GitHub's v4 service. Use a compatible `actions/upload-artifact@v3` or a verified Forgejo upload action, not an automatic upgrade to GitHub's v4 action. Upload diagnostics with `if: always()` when they are useful after failures. Keep optional reports best-effort without masking mandatory build failures.

### Publish static HTML with production and PR preview URLs

Build a complete static site into an output directory such as `out/`, including an `index.html`. The generator can be any toolchain; install its locked dependencies and run its build before publishing. Publish the generated directory with `actions/git-pages@v2`, not a GitHub Pages deployment action or a branch-push workaround.

The local hosting convention is:

- Production: `https://<owner>.pages.local.doreto.com.br/<repo>/`
- PR preview: `https://<owner>.pages-preview.local.doreto.com.br/<repo>@<number>/`

These are instance-specific internal domains. On another instance, confirm its Pages domains and authorization setup rather than copying them unchanged. Ensure generated links and asset paths work under the repository subpath, including the PR preview subpath.

Use the build triggers above and the following workflow-level settings and job pattern. Replace the illustrative build command with the project's actual generator:

```yaml
permissions:
  contents: write
  pull-requests: write

concurrency:
  group: publish-${{ github.event.pull_request.number || github.ref }}
  cancel-in-progress: true

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7
        with:
          ref: ${{ github.event.pull_request.head.sha || github.sha }}
          persist-credentials: false
      # Install the project's build tools and locked dependencies here.
      - name: Generate static HTML
        run: ./scripts/build-site --output out
      - name: Publish site
        uses: actions/git-pages@v2
        with:
          site: ${{ github.event_name == 'pull_request' && format('https://{0}.pages-preview.local.doreto.com.br/{1}@{2}/', github.repository_owner, github.event.repository.name, github.event.number) || format('https://{0}.pages.local.doreto.com.br/{1}/', github.repository_owner, github.event.repository.name) }}
          token: ${{ github.token }}
          source: out/
      - name: Comment with preview URL
        if: github.event_name == 'pull_request'
        uses: actions/comment-pull-request@v1
        with:
          message: |
            Preview published for commit ${{ github.sha }}:
            https://${{ github.repository_owner }}.pages-preview.local.doreto.com.br/${{ github.event.repository.name }}@${{ github.event.number }}/
          comment_tag: preview_page
          GITHUB_TOKEN: ${{ github.token }}
```

The stable `comment_tag` updates the existing preview comment instead of posting a new one on every push. Use the job token rather than a personal token where supported. Confirm the instance's actual token permissions and fork-PR publishing policy; YAML `permissions` alone is not proof of authorization. Do not work around denied previews by giving untrusted code stronger credentials. If manual runs should publish production only from the default branch, add an explicit event/ref guard so dispatching another branch cannot overwrite production.

If a public build dependency is a submodule with a developer-facing SSH URL, override that submodule's URL to its anonymous HTTPS endpoint before `git submodule update --init --depth 1`. Keep the pinned submodule revision; CI does not need developer SSH credentials.

Before finishing, check the workflow diff, verify the run with `fj actions ... --help` and the appropriate run/job inspection commands, and report the run result and published URL. Do not claim a successful deployment based only on valid YAML or a successful local build.

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
