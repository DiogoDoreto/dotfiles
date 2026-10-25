# On-demand host input updates and PR closure reports

Status: user-approved design, implemented locally; awaiting review and operator
rollout/pilot. The earlier pending-confirmation status is superseded by explicit
implementation approval. Workflows and helpers have been added; no credential
has been read/provisioned and no real branch, PR, deployment or activation has
been performed. Existing build/retention deployment is not evidence that these
new publishing/reporting paths have been live-tested.

## Input-update workflow

- One manual Forgejo workflow with a selector for `lapdog`, `mini`, `chungus`,
  or `all`. Inspiron7520 and the rescue image are excluded.
- Dispatch is allowed only from `main`. Capture its starting commit once; every
  selected host starts from that same commit and targets `main` in its PR.
- Update all flake inputs for each selected host. This workflow does not check,
  evaluate the system for validation, or build the host.
- Each changed host gets a fresh branch and PR on every dispatch. Do not find,
  refresh, deduplicate, or overwrite earlier update branches or PRs.
- If the lock file is unchanged, succeed with an already-up-to-date summary and
  publish neither a branch nor a PR for that host.
- Selected hosts proceed independently; one host's failure does not cancel the
  others.
- The initial PR describes the input update. Package reporting belongs to the
  existing build workflow, not this workflow.
- No system deployment or activation.

## Build-workflow reporting

Extend `.forgejo/workflows/nix-build.yml`, preserving its mandatory checking,
building, and closure-retention behavior documented in [nix-actions.md](nix-actions.md).

- Reporting applies to every trusted, same-repository PR accepted by that
  workflow, not only automated update PRs.
- Successful host builds compare their complete system closures with the same
  host's retained closure for the PR's target branch. Mini's NixOS output is
  `dogdot`; its host/report identity remains `mini`.
- Immediately before comparison, resolve the current target-branch tip. The
  retained baseline's provenance must match that exact commit. Never silently
  fall back to an older successful build or the runner's installed system.
- If the baseline is absent, stale, or unavailable, skip comparison with a
  visible Actions warning. Never build a baseline solely for reporting.
- Include additions, removals, version changes, and same-version rebuilds,
  including transitive dependencies. Identify both compared commit SHAs.
- Main pushes and manual build runs without a PR do not publish PR comments.

## Comment and artifact presentation

- Keep one bot-owned report comment per host per PR. Identify it with a stable
  hidden host-specific marker and verify authorship before editing it.
- Create the comment for the first available report; subsequent successful
  reports edit the same comment rather than adding comments.
- Include host, both commit SHAs, change-category counts, a bounded preview,
  and a link to the build run.
- Upload the complete report as an Actions artifact. Do not silently truncate
  the only complete copy. Artifact availability follows instance retention.
- Identical closures still produce a short no-system-closure-changes report.
- If a subsequent build cannot produce a comparison, preserve the earlier
  comment with its original SHAs and run link; warn in Actions instead of
  presenting it as a new report.
- All comparison, artifact, and comment failures are non-blocking and visible
  as Actions warnings. They must not mask actual check/build/retention failures.

## Automation identity and trust

- Use a dedicated automation account restricted to this repository, with the
  narrowest supported token permissions needed for branch pushes, PR creation,
  and comments. Verify actual Forgejo permissions during implementation.
- Store the token as a Forgejo Actions secret. Expose it only to publishing
  steps, not Nix commands. Do not assume the automatic PR job token can comment.
- A shared persistent runner is not a security boundary between steps. This
  design relies on the existing trusted-only repository/runner policy.
- Credential creation and provisioning are operator tasks. No credential has
  been requested, read, created, or installed during this design session.

## Implementation verification points

Dispatch, token, trigger and artifact implementation compatibility has been
checked against deployed Forgejo 16.0.6 and native runner 13.2.0 primary sources
and offline workflow validation; authenticated live publishing remains an
operator pilot. Existing trust gates, pinned checkout, exact-SHA checkout and
retention rules are preserved. The details and remaining limitations follow.

## Implemented interfaces

- `.forgejo/workflows/nix-input-update.yml`: manual dispatch, `host` choice
  defaulting to `all`. Select **main** in the UI. Non-main dispatches are skipped
  at the capture job; the helper also refuses non-main refs. Invalid API-supplied
  host choices fail before updating anything. A capture job exports the dispatch
  event's main SHA once and a selected-host matrix. Each independent host checks
  out that exact SHA, with `fail-fast: false`.
- `scripts/nix-input-updates/update.py prepare`: requires a clean checkout,
  runs `nix flake update --flake ./hosts/<host>` (no input names means all
  inputs), and accepts only that host's changed `flake.lock`. No checks, host
  validation/evaluation, builds, or baseline work. An unchanged lock produces an
  already-up-to-date log/summary and prevents the publishing step from running.
- `update.py publish`: accepts only the prepared changed lock at the captured
  SHA, commits just that lock, pushes a fresh
  `automation/inputs/<host>/<run-id>-<random-id>` branch without force, and creates
  a PR against main. It never looks up earlier update PRs. Publication failure
  fails that host, not the others. A push followed by PR API failure can leave an
  orphan branch for manual inspection; retries deliberately use fresh branches,
  not a recovery/deduplication mechanism.
- `report.py prepare`: runs only after successful mandatory PR build/promotion.
  Uses the automatic read-only job token for fixed-repository API reads. Under
  the existing promotion lock, resolves the current target tip, validates the
  retained target root/provenance with the existing retention code, requires its
  commit to equal that tip, and queries both **complete** closures with
  `nix-store --query --requisites`. Also verifies the candidate's host, branch,
  SHA and output provenance. Rechecks target tip, selection and PR head/ref after
  comparison, including candidate output/provenance revalidation. No baseline
  build or fallback to another root/installed system.
- Complete reports are `report.json` (both full closure inventories, provenance,
  categorized changes) and `report.md` (every change), uploaded as
  `host-package-report-<host>-<attempt>` on the run. Retention is the instance's
  default. Upload failure warns and prevents a new comment, preserving any
  earlier comment rather than publishing a preview without a complete artifact.
- `report.py publish`: the bot token is exposed **only** here (and the updater's
  publish step). It inventories every page of PR comments, matches the stable
  `<!-- nix-host-package-report:<host> -->` marker **and** configured bot author
  ID, then creates or edits one comment. Duplicate owned markers or corrupt
  freshness metadata fail closed with a warning. Other authors' markers and
  other hosts' reports are never edited.

### Report semantics and freshness

All paths added/removed from the complete closure appear exactly once in the
categories. Names are split using Nix's conventional name/version convention,
not claimed to be semantic package IDs. Only names occurring once in each
entire closure are paired as a version change or same-version rebuild; ambiguous
multiple versions/outputs remain explicit additions/removals. Shared paths do
not count as changes. Identical closures still produce a short report.

The comment identifies host, source SHA, exact target SHA, counts, build-run link
and artifact name. It contains at most 40 preview changes, with long preview
lines bounded, and has a 24,000-character final guard. Neither artifact is
truncated. The preview is escaped for Markdown/HTML; the JSON retains exact paths.

Publishing uses a per-host/PR `flock` in the same persistent runner state domain
as retention. Freshness metadata in the owned comment orders reports by
`(repository run_number, run_attempt)`; same/older attempts cannot replace a newer
report, including an old retry after a newer run. Immediately before mutation,
the helper rechecks that the PR is open, same-repository, still at the built head,
still targets the same ref, and that ref still has the compared SHA. A skipped or
failed comparison/artifact/comment path warns and leaves the previous report
with its original SHAs/run link. Main pushes and non-PR dispatches never comment.

Forgejo's comment API has no conditional-write/CAS interface. A head/target change
can still occur after the last API read; the comment truthfully names the actual
compared commits, rather than promising an atomic snapshot of changing remote
state. Local locking serializes publishers on the **existing single persistent
runner** (capacity 1); do not add another runner or independent publisher without
a shared-lock/CAS redesign. The lock is not a new multi-runner guarantee. Remote
ref observation has the same unavoidable final-observation race described for
retention in [nix-actions.md](nix-actions.md).

All reporting steps are best-effort, after mandatory check/build/promotion.
Helper exceptions produce escaped Actions warnings; action/startup/upload
failures have `continue-on-error` plus a separate warning step. No reporting
failure changes mandatory step results or releases/moves any retained root.

## Operator rollout (not performed by the implementation agent)

1. Reaffirm the existing private-instance, trusted-only runner policy in
   [nix-actions.md](nix-actions.md), including the server-side fork gate and
   repository-scoped runner. Adding a publishing token to a persistent native
   runner does **not** isolate it from arbitrary trusted repository code or
   earlier compromised jobs. Every writer/dispatcher and the automation account
   must be trusted. No `pull_request_target`, fork approval, automatic deployment,
   timer, runner registration, or retention-policy change is added.
2. Create/use a dedicated normal automation account with access only to
   `diogo/dotfiles`. Grant only repository code-write and necessary PR/comment
   permissions, not instance/org administration. Create a **repository-specific**
   personal access token restricted to this repository with **`write:repository`
   and `write:issue`**. These cover Git pushes/PR API and issue-comment API,
   respectively; no user/admin/Actions-management scope is needed. Forgejo 16
   rejects repository-specific tokens with `read:user`, so do **not** add that
   scope just to call `/user`.
3. Manually install that token as repository Actions secret
   **`NIX_AUTOMATION_TOKEN`**. Set repository Actions variable
   **`NIX_AUTOMATION_BOT_ID`** to the dedicated account's numeric user ID, verified
   by the operator from account/API metadata. The ID is not a secret. It is the
   authorship check, so an incorrect/missing value must be corrected before use;
   missing/invalid values cause reporting to warn and skip. Do not reuse the
   runner registration/authentication secret, maintenance read token, administrator
   token, or Harmonia key. Never put credentials in YAML, Git remotes, argv or
   Nix files. Rotation to a different bot ID requires manual disposition of old
   bot-owned comments; they are intentionally not taken over by another account.
4. Manually deploy the runner's small PATH addition (`python3`) while idle. No
   new reporting daemon or helper installation is required: the reviewed checkout
   supplies the Python scripts. They use only the Python standard library.
   Script subprocess environments are allowlisted; bot/job/runtime tokens and
   injected Git/Nix configuration are not forwarded to Nix. Only the Git push
   receives a fixed-repository authorization header via process environment,
   with redirects and credential-helper fallback disabled. Checkout credentials
   remain unpersisted. Artifact upload uses the runner's ephemeral runtime token,
   never the bot token.
5. On reviewed main, manually pilot a single host, then `all`: verify exact captured
   SHA, unchanged-host no-op, independent failure handling, fresh PR/branch per
   dispatch/retry, and bot-created PRs triggering the existing trusted build
   workflow. Verify actual token restrictions and private-repository API reads;
   source compatibility is not a live permission test.
6. Verify a main build has retained current-tip roots before expecting comparisons.
   In a trusted PR test all host reports, complete downloadable artifacts, later
   comment edits, unchanged closures, stale/absent baseline warnings, target/head
   advancement and old-run retries, artifact/comment permission errors preserving
   the old report, and mandatory build failure remaining a failure. Inspect actual
   native-runner artifact storage/TLS, expiry/download permissions and cancellation.

## Verification performed locally

- Deployed read-only `/api/v1/version`: **16.0.6**. Public
  `/swagger.v1.json` advertises PR, branch, issue-comment and manual workflow
  dispatch endpoints. No bot token or other secret was read or provisioned.
- Native runner **13.2.0** package only was realized (no host system build).
  Both workflows pass `forgejo-runner validate --workflow --path ...` and
  `forgejo-runner exec --list -W ...`. This validates schema/planning, not live
  execution, dispatch selection, job status, or artifact upload.
- `python3 -m unittest discover -s scripts/nix-input-updates/tests -v`:
  **29 tests passed**. Includes lock-only/no-op publishing guards, clean/base/host
  rejection, credential stripping, disposable local Git pushes, exact diff
  coverage/ambiguities, full vs bounded presentation, API errors/pagination,
  corrupt metadata, current-tip/provenance failures, head/target changes during
  comparison and before publication, old-run/retry ordering, and concurrent
  publishers. Forgejo and updater Nix operations are fakes; no real publication.
- Existing retention/maintenance suite: **62 tests passed** with
  `NIX_CACHE_ROOTS_REAL_NIX=1`, including isolated-store GC tests under
  `.private/tmp` (no production GC). The default run passes with 25 real-Nix tests
  skipped. A read-only real `nix-store` query of the runner package returned its
  35-path complete closure; identical-closure comparison passed.
- `make format-nix` and `git diff --check` passed. Enabled Chungus runner
  `hostPackages` evaluation confirms Python 3 alongside existing packages.
  `nix flake check ./hosts/chungus/ --no-build --no-update-lock-file
  --no-write-lock-file` was attempted but blocked during unrelated
  `snapraid-btrfs-sync` evaluation by invalid/missing store derivation
  `ak82jhy5wws7gqwxhx9ka309jpgsddg5-source.drv`; no full Chungus system was built.
  An initial standalone-host `getFlake` expression also failed because it lost
  the repository-relative `nix-config.nix`; the corrected repository Git-flake
  expression for the focused enabled-module evaluation passed.

### Primary-source compatibility references

- [Forgejo 16 workflow reference](https://forgejo.org/docs/v16.0/user/actions/reference/):
  manual `choice`/`options`, inputs context, needs outputs, matrices,
  `continue-on-error`, run number/attempt and context aliases.
- Forgejo **v16.0.6**:
  [dispatch service](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/actions/workflows.go)
  captures `entry.Commit` at dispatch; selector validation is also enforced in
  our shell/helper because server API inputs are not assumed to enforce choices.
  [token resource validation](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/authz/access_token.go)
  permits repository-specific tokens only with issue/repository scopes;
  [API route groups](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/routers/api/v1/api.go)
  assign those scope categories to PR and issue-comment endpoints. The
  [access-token authentication method](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/auth/method/access_token.go)
  accepts PATs via Basic for Git and Token/Bearer for API requests, independently
  of the chosen Basic username; actual bot permissions remain a pilot check.
- [Actions notifier](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/actions/notifier.go)
  receives new-PR events;
  [notifier helper](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/actions/notifier_helper.go)
  suppresses special `IsActions()` users, **not** a normal dedicated account's PAT
  events. This is why the automatic workflow token is not used for PR publication.
  Trusted-account configuration/actual bot PR triggering still require the pilot.
- [Runner v13.2.0 runner.go](https://code.forgejo.org/forgejo/runner/src/tag/v13.2.0/internal/app/run/runner.go)
  supplies `ACTIONS_RUNTIME_URL`/`ACTIONS_RUNTIME_TOKEN` to native executions and
  passes run number/attempt context. Forgejo's
  [artifact routes](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/routers/api/actions/artifacts.go)
  and router initialization expose the compatible v3 artifact protocol.
  The workflow pins Forgejo's
  [upload-artifact v3-node20 action](https://code.forgejo.org/forgejo/upload-artifact/src/commit/97a0fba1372883ab732affbe8f94b823f91727db/action.yml)
  at `97a0fba1372883ab732affbe8f94b823f91727db` (node20); inspected action metadata
  and bundled v3 client use that protocol. Existing local checkout mirror pin
  and exact-SHA/unpersisted-credential settings are unchanged.
