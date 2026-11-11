# Proposed stdlib file store and implementation seams

**Proposed deviation from the earlier SQLite direction:** use Linux-local private
files, atomic replacement and `flock`, avoiding SQLite/CGO or a pure-Go SQL driver.
The user allowed this tradeoff. There is no store implementation in this commit.
Do not describe the proposal as offering SQLite's multi-record transactions.
Reassess if future requirements need concurrent writers, queries or remote storage.

## Host-private layout and ownership

Suggested default root: `/home/dog/.local/state/agent-host-review`, owned by dog,
mode 0700; files 0600 and umask 077. This is **outside** `/home/dog/projects` and
`/var/lib/agent-vm` and must not be shared into the Agent VM through any mount,
symlink, SSH command, or other export. Validate the actual deployed paths, owners
and guest-writable ancestors rather than relying on these example strings. Raw
capture belongs in a separate private `raw/` tree so release publication and
retention cannot accidentally export it. Never use guest-selected paths here.

```
store.lock                  stable inode; never rename/unlink as locking protocol
sessions/<host-session-id>/  private session metadata (not browser capabilities)
snapshots/<host-id>.json     immutable original validated guest proposal + host provenance
records/<host-id>.json       authoritative whole request record, revision, phase,
                            frozen spec/digest, approval/spent nonce and result refs
raw/<host-id>/               private capture and supervisor result, not guest readable
releases/<artifact-id>.json  immutable reviewed release; guest transfer allowlist only
```

IDs are validated contract IDs, not arbitrary path components. A host-generated
snapshot ID must never be derived from guest request ID or guest filenames. Store
both IDs and content digest in provenance, while treating all guest assertions as
untrusted. Bound files/directories/queue length and disk usage, not only individual
message sizes. Separate root-owned transient execution handoff/runtime directories
may be required by the root proof; a dog-owned ledger cannot itself authenticate
inputs to a privileged helper. Do not decide that boundary by copying this layout.

## File transaction/crash protocol

* Open a private trusted directory, check ownership/mode/type and reject symlinks
  and unexpected hard links. Use directory-relative safe opening and `O_NOFOLLOW`
  where appropriate; never check-then-follow a guest path. Even a stdlib store needs
  careful Linux path handling; do not claim `os.Rename` alone solves traversal.
* All cooperating host readers/writers use the stable `store.lock` with Linux
  `syscall.Flock`. Hold exclusive lock for revision checks, nonce spend, publication,
  cancellation races and GC. `flock` is advisory, not a defense against malicious
  host dog/root; they already have execution authority. Do not use network filesystems.
* Write a bounded whole JSON record to a fresh 0600 temp in the **same directory**;
  `Sync` the file, close it, rename over the authoritative record, then `Sync` the
  parent directory. Error at any stage fails closed; do not execute on uncertain
  durability. Readers parse strictly and validate version/invariants.
* Immutable snapshots/releases must use no-clobber publication, not replacement.
  A possible stdlib Linux pattern: sync a fresh temporary file then `os.Link` it
  to the final host-ID name (fail if exists), sync the directory, unlink the temp
  and sync again. Recovery cleans orphan temps only under the lock. Validate this
  against the actual filesystem and fault-inject every step before shipping.
* No cross-file atomic transaction: publish immutable snapshot/artifact first,
  then durably replace the record that references it. A crash can leave orphan
  immutable files, never an advertised artifact that wasn't published. GC must
  tolerate and later reclaim orphans. Snapshot creation and record creation use
  the same lock; recovery repairs/rejects incomplete imports, never executes them.
* Nonce consumption is part of the **single record replacement**, never a separate
  spent-token file plus mutable phase update. No deletion/unspend on prompt denial.
  The record retains the consumed binding even when awaiting a new approval.
  Browser retry cannot launch twice; crash after consumption cannot auto-retry.
* Global one-at-a-time is checked under the same lock over authoritative records
  (starting and running both reserve the slot). Missing/corrupt/incompatible records
  block new execution until repaired, not ignored as empty. A separate derived
  index/cache must never decide authorization or concurrency.
* Immutable bytes for a release are published before changing phase to released.
  A crash before the phase change keeps them withheld. Only a released ledger
  reference may be transferred; a directory listing is not a release allowlist.
  SSH retries return exactly those bytes. Never fetch files using a guest path.
* Power loss can still leave stale records if a filesystem/hardware stack does
  not honor fsync. Atomic replacement prevents partial JSON exposure; it does not
  guarantee execution exactly once. Ambiguous intent is terminal, never replayed.

The future guest queue uses the same bounded JSON/atomic-file/lock pattern in its
own 0700 directory. Its locks protect cooperative processes only: guest root can
alter it all. Queue contents, leases and cancellation markers convey requests,
not host approval. The host maintains its own dedupe record and immutable copy.

## Guest worker seam

* MCP tools submit/check/cancel, mapping to v1 Request and Reply. No execution,
  root prompts, approval capabilities, or host-side listening socket in the VM.
* Reject submissions without an active host-renewed reviewer lease. A compromised
  guest can lie, but cannot mint host approval. Expired routes cannot accumulate
  work for the next session. Bounded queue, requests and replies, deliberate
  deduplication and fixed coarse errors; local parser errors never leak host data.
* Expose a **fixed absolute executable** remotely over SSH for bounded JSON stdin /
  stdout. Transport framing/host pull verbs (lease renewal, enumerate/import,
  acknowledge, released delivery) belong in a separately versioned transport
  envelope in the guest/host worker, not arbitrary extra fields in Request.
  The current Request is the tool/queue record, **not** a finished SSH RPC protocol.
* Stdout is protocol-only; stderr is diagnostics. Do not return guest-provided
  paths for host opening; transfer bounded values as data. A malicious executable
  can block, spam or return hostile output; host must enforce deadlines and byte
  caps independent of guest behavior. Never trust guest counts or advertised sizes.
* Later Pi native MCP integration owns one named entry. Merge/update only that
  entry, preserving unrelated entries; do not replace the complete user config.
  No Pi config or extension wiring is implemented here.

## Host worker seam

* Start manually in local Plasma; own local session/epoch and singleton lock.
  Pull guest proposals via `os/exec` OpenSSH argv, reusing the existing host
  maintenance key. Use a **separate strict-pinned known_hosts**,
  `StrictHostKeyChecking=yes` and deliberate enrollment (no accept-new/TOFU).
  Disable fallback/password/interactive auth and uncontrolled SSH config effects;
  make host/user/port/identity/known_hosts and the fixed remote executable explicit.
* SSH runs a fixed remote absolute command, never interpolation of script, cwd,
  ID, or other untrusted values into the remote command string. JSON goes through
  bounded stdin. Choose fixed remote command tokens without shell metacharacters.
  Request IDs remain data even though their syntax is constrained.
* Host validates each reply and session binding, then copies into a host-private
  immutable snapshot before display. Edits only change the host review record;
  guest files cannot mutate an existing approved spec. Guest cancellation is a
  request serialized by the host; it cannot rewrite an execution outcome.
* Host alone decides admission, approval, nonce spend, execution, cancellation,
  output release, crash ambiguity, retention and supervisor lifetime. Browser
  buttons and pure `Next`/`Validate` methods confer no authority by themselves.
* Never run SSH or wait on a prompt while holding the store lock. Reserve intent
  durably under lock first, release it, then invoke external processes. Reconcile
  under lock afterward. All fault paths preserve spent intent.

## Web worker seam

* Bind loopback only, authenticated with a fresh browser capability delivered
  in URL **fragment**, exchanged into a session, and removed from address/history
  immediately. Never put it in query strings, logs, Referer or guest messages.
  Subsequent requests use an authenticated session and CSRF protection, not the
  routing ID. Use cryptographic constant-time capability comparisons.
* Strict exact Host allowlist for bound address/port; strict Origin checks and
  CSRF tokens for mutation; do not enable permissive CORS or trust proxy headers.
  Reject unexpected content types, methods, body size and redirect destinations.
  Protect initial capability exchange from cross-site requests as well.
* Use stdlib `net/http`, `html/template` / `encoding/json`, and bundled `embed`
  assets; no frontend package ecosystem. Render scripts, metadata, errors and
  output as escaped **plain text**, never raw HTML/Markdown/ANSI terminal content.
  Bundle a restrictive CSP, no remote assets, no cache of secrets; protect browser
  session cookies with HttpOnly/SameSite and an explicit loopback HTTP policy.
* Show full frozen spec, user/root, revision/digest, timeout, metadata disclosure
  warning and redactions. Disable new approvals when browser liveness expires;
  server remains the authority even when UI state is stale. Recheck before consume.
* Approval and release are separate authenticated actions. Neither polling nor
  successful execution releases data. Test DNS rebinding/Host confusion, CSRF,
  stale revisions/nonces, reconnection, hostile text and secret-bearing metadata.

## Required later validation

Before deployment add storage fault-injection at every fsync/rename/link boundary;
concurrent approval/cancel/retry tests; lease expiry and disconnect tests; suspend /
resume/session expiry and reviewer death tests; SSH spoofing, mutable queue and
oversized/blocked peer tests; root denial/timeout/cgroup/orphan tests; combined
output draining/truncation tests; and raw/release retention tests. Contract unit
tests are intentionally not evidence those mechanisms work.
