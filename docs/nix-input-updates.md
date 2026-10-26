# On-demand host input updates and closure reports

The manual input updater and best-effort Host package change report helpers are
implemented. Reports now use **Forgejo job summaries**, not PR comments. No
credential provisioning, real PR publication, host deployment or activation is
performed by local implementation/testing. Operator rollout remains manual.

## Input-update workflow

- `.forgejo/workflows/nix-input-update.yml` is manually dispatched with `lapdog`,
  `mini`, `chungus`, or `all`. Inspiron7520 and the rescue image are excluded.
- Dispatch is allowed only from `main`. Capture its starting commit once; every
  selected host starts from that same commit and targets `main` in its PR.
- Update all flake inputs for each selected host. The updater does not check,
  evaluate the system for validation, build the host, or build a baseline.
- Each changed host gets a fresh branch and PR on every dispatch, including
  retries. Do not find, refresh, deduplicate or overwrite earlier update PRs.
- An unchanged lock produces an already-up-to-date summary and no branch or PR.
- Hosts proceed independently (`fail-fast: false`). Only the selected host's
  `flake.lock` may change. Publishing requires the captured base SHA and a clean
  checkout apart from that prepared lock change.
- The initial PR describes the input update. Package reporting belongs to the
  existing build workflow, not this workflow. No system deployment/activation.

### Updater interfaces

`update.py prepare` runs `nix flake update --flake ./hosts/<host>` without input
names, after validating host/base/clean checkout. `update.py publish` commits
only the prepared lock, pushes a fresh
`automation/inputs/<host>/<run-id>-<random-id>` branch without force, and creates
a PR against main. A push followed by API failure can leave an orphan branch;
retries deliberately use a new branch rather than recover/deduplicate it.
Invalid API-supplied host choices fail before updating anything.

## Build-workflow reporting

`.forgejo/workflows/nix-build.yml` preserves mandatory checking, building and
closure retention from [nix-actions.md](nix-actions.md). Reporting runs only after
successful mandatory PR build/promotion, for every trusted same-repository PR
accepted by that workflow, not only automated input updates. Main pushes and
manual non-PR builds do not generate comparisons.

`report.py prepare` compares each host's **complete system closure**, including
transitive dependencies, with the same host's retained target-branch closure.
Mini's NixOS output is `dogdot`; its report identity remains `mini`.

Under the existing promotion lock, the helper:

1. Resolves the current PR target-branch tip (not the event's possibly stale SHA).
2. Validates the retained root/provenance and requires its commit to equal that
   exact tip. Missing/stale baselines warn and skip; there is no older-root or
   runner-installed-system fallback and no baseline build.
3. Validates the candidate's repository, branch, host, source SHA and output.
4. Queries both closures using `nix path-info --json --recursive`, validating
   each path and nonnegative integer `narSize`. Supports path-keyed object and
   path-bearing list JSON formats. Shared paths must have identical sizes.
5. Rechecks target tip, selected root, candidate output/provenance, and PR
   open/same-repository/head/target identity before releasing the lock.

No reporting action moves/releases retained roots or changes retention policy.
The read-only job token is used only for fixed-repository API reads; it is not
forwarded to Nix subprocesses.

## Grouped presentation and sizes

The summary is inspired by `nh`, with **Changed**, **Added**, and **Removed**
sections. Paths are grouped by Nix's conventional name/version split, not a
semantic package identity. A name present on both sides is Changed when its
paths differ; a name present on only one side is Added or Removed. Multiple
versions/outputs are displayed together rather than asserted to be upgrades.

- Rows show before/after version or output labels, multiplicity (`×2`, etc.),
  shared paths collapsed to `<unchanged>`, and net NAR size delta.
- Versionless names (such as units) display `<none>`.
- PATHS shows unique before/after path counts and added/removed counts.
- SIZE shows the sum of each unique path's NAR size on each side; DIFF is its
  signed difference. These are **NAR sizes, not physical disk usage**, download
  sizes, or deduplicated storage consumption.
- Per-group size deltas reconcile exactly to the total delta (displayed values
  are rounded binary units). Shared paths contribute no delta.
- Identical closures produce a short no-system-closure-changes message and totals.

### Complete artifacts

The run uploads `host-package-report-<host>-<attempt>` containing:

- `report.json`: schema version **2**, host/run/PR/commit provenance, both complete
  sorted path inventories, a `nar_sizes` mapping in bytes, exact `changes`, and
  `groups` with full before/after paths and integer-byte deltas.
- `report.md`: all grouped rows and all exact changed paths, without truncation.

The exact-path audit categories remain additions, removals, version changes and
same-version rebuilds. Only names occurring once in each entire closure are
paired as version changes/rebuilds; ambiguous versions/outputs remain explicit
additions/removals in these categories. Every added/removed path appears exactly
once. Grouped presentation does not alter those conservative audit semantics.
Artifact availability follows instance retention.

## Job summaries (no PR-comment writes)

After the artifact step, `report.py summary` appends Markdown to
`FORGEJO_STEP_SUMMARY`, falling back to `GITHUB_STEP_SUMMARY`. It includes host,
exact compared SHAs, build-run link, artifact name, totals and at most **40 grouped
rows**. Long preview names/versions are bounded and escaped for Markdown/HTML.
A final **240,000-byte UTF-8 guard** stays below Forgejo's 1 MiB per-step limit;
the complete artifacts are never silently truncated.

- Artifact upload failure still produces the preview, clearly marked **complete
  artifact unavailable**; no successful-download claim is made.
- An unavailable comparison produces a short explanation in the summary and an
  Actions warning. Failure to start the preparation helper produces a generic
  explanation rather than reusing stale report files from an earlier attempt.
- A missing summary environment or summary write failure emits an Actions
  warning. Reporting infrastructure failures are also guarded by workflow
  `continue-on-error` and a warning step. None masks mandatory build failures.
- Each summary belongs to its own host job/run. There are no bot-comment markers,
  comment freshness ordering, publication locks, author checks or comment API
  writes. Existing PR comments are left untouched.
- The build workflow needs neither `NIX_AUTOMATION_TOKEN` nor
  `NIX_AUTOMATION_BOT_ID`. The updater still needs its publishing token.

These are truthful snapshots of the identified commits, not promises of an
atomic snapshot of remote refs. Forgejo refs can change after the final API
observation. Unlike a shared comment, an older run's summary cannot overwrite
another run's report.

## Automation identity and trust

The updater uses a dedicated normal automation account restricted to this
repository, with repository-specific token scopes **`write:repository` and
`write:issue`** for Git pushes/PR creation. No user/admin/Actions-management scope
is required. Forgejo 16 rejects repository-specific tokens with `read:user`; do
not add it to call `/user`. Store the token as `NIX_AUTOMATION_TOKEN` and expose
it only to the updater's publish step. `NIX_AUTOMATION_BOT_ID` is no longer used.

The automatic job token is not used for updater publication: dedicated normal
account PAT events can trigger the trusted build workflow, unlike special
Actions-user events. Actual permissions/event behavior still need a live pilot.

A persistent native runner is **not** a security boundary between steps. All
writers/dispatchers and the automation account must be trusted. See the existing
server-side fork gate and repository-scoped runner policy in
[nix-actions.md](nix-actions.md). No `pull_request_target`, fork approval,
automatic deployment, timer or runner-registration change is added.

Subprocess environments are allowlisted: bot/job/runtime tokens and injected
Git/Nix configuration are not forwarded to Nix. Only the updater Git push receives
a fixed-repository authorization header via environment, with redirects and
credential-helper fallback disabled. Checkout credentials remain unpersisted.
Artifact upload uses the runner's ephemeral runtime token, never the bot token.

## Operator rollout and compatibility

1. Reaffirm the trusted-only private-instance runner policy. Manually provision
   the updater's dedicated account/token if not already present. Never reuse
   registration, maintenance, admin or Harmonia credentials. Credentials are
   not needed to publish summaries.
2. Ensure Python 3 remains available on the runner. Helpers come from the
   reviewed checkout; only the Python standard library is required. No new
   `nh`/`nvd` dependency or report daemon is introduced.
3. Verify **end-to-end summary rendering on the deployed server and runner**.
   Read-only server metadata reports Forgejo **16.0.6**. Runner **13.2.0** source
   supplies step-summary files and processes summary commands with a 1 MiB cap.
   **Upgrade the server to Forgejo 17 or newer before relying on summaries.**
   The summary storage model (`models/actions/task_step_summary.go`) is present
   in v17.0.0 but absent in v16.0.6, and the v16 advanced-features page has no
   summary documentation. Schema validation is not proof of server/UI support.
   There is deliberately no PR-comment fallback.
4. Manually pilot the updater on reviewed main for one host, then all. Verify
   unchanged-host no-op, independent failures, exact base SHA, fresh branches/PRs,
   token restrictions and bot PR build triggers.
5. Ensure main builds have retained current-tip roots. In a trusted PR, verify
   all host job summaries and complete artifacts, retry-local summaries,
   unchanged closures, stale/absent baseline explanations, head/target advances,
   upload failures and summary write failures. Confirm mandatory build failure
   remains a failure. Inspect actual artifact storage/TLS, expiry and permissions.

## Local verification

```sh
python3 -m unittest discover -s scripts/nix-input-updates/tests -v
python3 -m unittest discover -s scripts/nix-cache-roots/tests -v
git diff --check
forgejo-runner validate --workflow --path .forgejo/workflows/nix-build.yml
forgejo-runner exec --list -W .forgejo/workflows/nix-build.yml
```

Tests cover grouping/multiplicity, exact path coverage, NAR accounting, metadata
validation, escaping/bounds, summary destinations, unavailable artifacts and
comparisons, provenance changes, credential stripping, updater lock-only behavior,
and disposable local Git pushes. Nix/API operations in these tests are fakes.
The retention suite skips real-Nix tests unless `NIX_CACHE_ROOTS_REAL_NIX=1`.
Local workflow validation does not publish anything or prove live rendering.

For this change, the input-update/report suite passed **40 tests**; the retention
suite passed **62 tests**, with 25 real-Nix tests skipped. Runner 13.2.0 workflow
schema validation and job planning passed. A separate real-Nix smoke test added
a tiny file to a disposable store rooted under `.private/tmp`, validated its
path-keyed NAR metadata (144 bytes), and verified an identical comparison. Queries
of the mounted runner/system Nix packages failed because their paths were not
registered in this environment's store; no host build or production GC ran.

### Compatibility references

- [Forgejo job summaries](https://forgejo.org/docs/latest/user/actions/advanced-features/#job-summaries)
- [Forgejo 17 summary storage model](https://codeberg.org/forgejo/forgejo/src/tag/v17.0.0/models/actions/task_step_summary.go)
- [Runner 13.2.0 step-summary handling](https://code.forgejo.org/forgejo/runner/src/tag/v13.2.0/act/runner/step.go)
- [Forgejo 16 workflow reference](https://forgejo.org/docs/v16.0/user/actions/reference/)
- [Forgejo 16 token validation](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/authz/access_token.go)
- [Forgejo 16 Actions notifier](https://codeberg.org/forgejo/forgejo/src/tag/v16.0.6/services/actions/notifier.go)
- [Pinned v3 artifact action](https://code.forgejo.org/forgejo/upload-artifact/src/commit/97a0fba1372883ab732affbe8f94b823f91727db/action.yml)
