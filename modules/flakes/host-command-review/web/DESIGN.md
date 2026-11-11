# Command review: design notes

## The decisions this screen supports

1. **May this command run on my host?** Read the guest's reason, inspect the actual
   command, directory, user and time limit. Expand execution policy for the frozen
   environment and shell/capture rules. Save edits before approving; saving never
   executes. Root uses a distinct action and KDE authentication, not a web password.
2. **May this result leave my host?** Inspect held output and exit/capture status.
   Edit only supported redaction fields, preview the entire export as readable
   content, then explicitly confirm disclosure. Withhold sends nothing, clears
   any pending confirmation and keeps the result private.
3. **Does this work need intervention?** Running work has a stop action, with a
   warning that stopping cannot undo side effects. An uncertain outcome asks for
   host investigation rather than offering a dangerous retry.

Reasons, not IDs, are titles. Reasons remain untrusted plain text. Hashes and
capabilities still participate in the unchanged protocol but are not UI content.
Recent terminal requests are collapsed instead of competing with actionable work.
Repeated frozen command/JSON dumps have been replaced by editable fields and a
policy disclosure. Preview intentionally repeats export content because that is
an independent disclosure decision, but confirmation does not dump it a third time.

## Iteration and checks

- Replaced the warning-heavy header and undifferentiated button list with a
  compact authority notice, reason-led queue and clear review panel.
- Browser inspection showed command controls were too far below the fold. Moved
  revision into policy and grouped directory/time/user controls on wider screens.
- Put preview before release, and confirmation after the preview. Withhold now
  invalidates a pending release instead of merely changing a status message.
- Removed redundant pre-run cancel (reject already handles that decision).
- Disabled unchanged saves and added local input checks to avoid pointless host
  round trips. Backend validation remains authoritative.
- Polling no longer rebuilds an unchanged queue, preserving keyboard focus and
  expanded history. Queue buttons resolve the latest polled request when clicked,
  not the stale object captured when they were created.
- Substantive changes preserve but disable old forms and provide a fresh-review
  action. Offer-only renewal preserves command drafts and output redactions.
- Added readable terminal-state messages, empty states, visible focus styles and
  a narrow single-column layout.
- Exercised actual Chromium edit/run/held/redaction/preview/release flows, root
  action distinction, withhold and 390px overflow checks. Dependency-free DOM
  tests cover authority changes, exact bindings, byte preservation and interlocks.

## Further improvements worth considering

These are follow-up opportunities, not claims that they were implemented:

- **Draft switching:** warn before leaving a dirty request, or retain separate
  in-memory drafts keyed to exact substantive review state. Never automatically
  restore a draft onto a new revision or persist host-private text in storage.
- **Long/binary output:** add find-in-output and an explicit non-UTF-8 warning
  with byte-oriented inspection. The current readable view replacement-decodes
  invalid UTF-8; unchanged streams still release the original exact bytes. Do not
  imply that rendered UTF-8 is a lossless byte inspection.
- **Redaction comparison:** a structured before/after view could make accidental
  disclosures easier to catch. It must distinguish stream replacement from
  changes to execution metadata and invalidate itself on every edit.
- **Busy and connection states:** granular per-action progress and a visible
  lease countdown would explain delays and fail-closed approvals better. Do not
  retry ambiguous network failures automatically.
- **Queue scale:** filtering by command/output review, counts, and bounded reason
  previews would help a busy queue. Avoid adding bulk run or bulk release: each
  command and disclosure deserves an independent review.
- **Production acceptance:** test keyboard/screen-reader navigation, actual CSP
  and origin enforcement, browser suspension/disconnect, multi-tab behavior and
  real KDE authentication. The design sandbox cannot validate host authority or
  process supervision. Timestamps, durable withholding and richer stop outcomes
  need backend support and are deliberately outside this web-only change.
