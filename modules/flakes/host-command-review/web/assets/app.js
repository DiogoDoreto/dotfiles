'use strict';
// No storage, cookies, eval, HTML interpretation, guest links, or mutation GETs.
const bootstrap = location.hash.slice(1);
history.replaceState(null, '', '/');
let auth = '', csrf = '', selected = '', current = null, leaseUntil = 0, busy = false;
let stale = false, bindingView = null, queueSignature = '', latestRequests = [];
const phaseNames = {reviewing: 'Review command', approved: 'Ready to run', starting: 'Starting', running: 'Running', output_held: 'Review output', ambiguous: 'Check outcome', released: 'Released', rejected: 'Rejected', cancelled: 'Cancelled'};
const actionable = r => ['reviewing', 'approved', 'starting', 'running', 'output_held', 'ambiguous'].includes(r.phase);
function meta(parent, label, value) {
  const row = node('div', undefined, parent); row.className = 'meta-row';
  node('dt', label, row); node('dd', String(value ?? 'Not provided'), row);
}
function policy(spec, parent) {
  const dl = node('dl', undefined, parent);
  meta(dl, 'Run as', spec.user); meta(dl, 'Directory', spec.cwd);
  meta(dl, 'Time limit', `${spec.timeout_seconds} seconds`);
  meta(dl, 'Shell', spec.shell); meta(dl, 'Stop grace', `${spec.stop_grace_seconds} seconds`);
  meta(dl, 'Output limit', `${spec.capture_bytes} bytes`);
  for (const [key, value] of Object.entries(spec.environment || {})) meta(dl, key, value);
  return dl;
}
function exportView(artifact, parent) {
  parent.replaceChildren();
  node('h3', 'What the guest will receive', parent);
  node('p', 'Transport identifiers are retained unchanged. All content and execution metadata are shown below.', parent);
  node('h4', 'Executed command', parent); node('pre', artifact.executed.script, parent);
  policy(artifact.executed, parent);
  meta(parent, 'Exit', artifact.exit ? `${artifact.exit.kind} · code ${artifact.exit.code} · signal ${artifact.exit.signal}` : 'Not provided');
  meta(parent, 'Output truncated', artifact.truncated ? 'Yes — capture is incomplete' : 'No');
  meta(parent, 'Redacted fields', artifact.redacted_fields.length ? artifact.redacted_fields.join(', ') : 'None');
  for (const name of ['stdout', 'stderr']) {
    node('h4', name === 'stdout' ? 'Standard output' : 'Standard error', parent);
    node('pre', decode(artifact[name]) || '(empty)', parent);
    meta(parent, 'Stream size', `${atob(artifact[name]).length} bytes`);
  }
}
const status = document.querySelector('#status');
const detail = document.querySelector('#detail');
function node(tag, text, parent) {
  const n = document.createElement(tag);
  if (text !== undefined) n.textContent = text;
  if (parent) parent.append(n);
  return n;
}
async function api(path, body) {
  const headers = {Authorization: `Bearer ${auth}`};
  const options = {method: body === undefined ? 'GET' : 'POST', headers, cache: 'no-store', credentials: 'omit', redirect: 'error'};
  if (body !== undefined) {
    headers['Content-Type'] = 'application/json'; headers['X-CSRF-Token'] = csrf;
    options.body = JSON.stringify(body);
  }
  const response = await fetch(`/api/${path}`, options);
  if (!response.ok) throw new Error(`Host refused request (${response.status}). Refresh and review again; no automatic retry.`);
  return response.json();
}
async function heartbeat() {
  if (!auth || document.visibilityState !== 'visible') { leaseUntil = 0; return; }
  try { const r = await api('heartbeat', {}); leaseUntil = Date.now() + r.lease_seconds * 1000; }
  catch (e) { leaseUntil = 0; status.textContent = e.message; }
}
function button(label, action, parent, disabled = false) {
  const b = node('button', label, parent); b.disabled = disabled;
  b.addEventListener('click', action); return b;
}
function field(label, value, parent, multiline = false) {
  const l = node('label', label, parent); const n = node(multiline ? 'textarea' : 'input', undefined, l); n.value = value; return n;
}
async function apply(action, extra = {}) {
  if (busy || !current || stale) return;
  if (Date.now() >= current.binding.expires_unix * 1000) { status.textContent = 'Review binding expired. Refresh to renew it; unfinished edits are preserved.'; return; }
  if ((action === 'run' || action === 'run_root') && Date.now() >= leaseUntil) { status.textContent = 'Browser lease expired. Reconnect and review again.'; return; }
  busy = true;
  // Copy the exact host-provided binding, never build an approval in the browser.
  const binding = structuredClone(current.binding);
  try { await api('operation', {action, binding, ...extra}); await refresh(true); }
  catch (e) { status.textContent = e.message; }
  finally { busy = false; }
}
function render(r) {
  current = r; stale = false; detail.replaceChildren();
  status.textContent = r.phase === 'output_held' ? 'Output is private. Review, redact if needed, then preview before release.' : ['reviewing', 'approved'].includes(r.phase) ? 'Review the command before running. Save any edits first.' : (phaseNames[r.phase] || r.phase);
  node('p', phaseNames[r.phase] || r.phase, detail).className = 'eyebrow';
  node('h2', r.reason || 'No reason supplied by the guest', detail);
  node('p', 'Reason supplied by the guest · untrusted text', detail).className = 'muted';
  const advanced = node('details', undefined, detail); node('summary', 'Execution policy', advanced);
  bindingView = node('p', `Revision ${r.binding.revision || 1} · Frozen by the host`, advanced); bindingView.className = 'muted';
  policy(r.spec, advanced);
  node('p', 'bash --noprofile --norc /proc/self/fd/3; immutable script FD3, never script argv; stdin /dev/null; no PTY. One command at a time, including root authentication. Session maximum: 4 hours. Root authentication happens in KDE, never here.', advanced);
  const locked = ['starting', 'running'].includes(r.phase);
  if (locked) node('strong', 'Read-only while authenticating/executing. Cancellation requests stop the process tree; they cannot undo side effects. Output stays held.', detail);
  if (['reviewing', 'approved'].includes(r.phase)) {
    const fs = node('fieldset', undefined, detail); node('legend', 'Edit then save; saving invalidates the old approval', fs);
    const script = field('Script', r.spec.script, fs, true);
    const options = node('div', undefined, fs); options.className = 'command-options';
    const cwd = field('Working directory', r.spec.cwd, options);
    const timeout = field('Time limit (seconds)' , r.spec.timeout_seconds || 120, options); timeout.type = 'number'; timeout.min = '1'; timeout.max = '1800';
    const userLabel = node('label', 'Execution user', options); const user = node('select', undefined, userLabel);
    for (const name of ['dog', 'root']) { const o = node('option', name, user); o.value = name; } user.value = r.spec.user;
    const approve = button(r.spec.user === 'root' ? 'Run as root — KDE confirmation' : 'Run as dog', () => {
      if (confirm(`Execute this exact frozen revision on lapdog as ${r.spec.user}? All output will be held.`)) apply(r.spec.user === 'root' ? 'run_root' : 'run');
    }, detail);
    if (r.spec.user === 'root') approve.className = 'danger';
    const dirty = () => { approve.disabled = true; save.disabled = false; status.textContent = 'Unsaved changes. Save the command, then review the updated version before running.'; };
    for (const input of [script, cwd, timeout, user]) input.addEventListener('input', dirty);
    const save = button('Save edited command (does not run)', () => {
      if (!script.value.trim() || !cwd.value.startsWith('/') || !Number.isInteger(Number(timeout.value)) || Number(timeout.value) < 1 || Number(timeout.value) > 1800) {
        status.textContent = 'Enter a command, an absolute directory, and a whole-number timeout between 1 and 1800 seconds.'; return;
      }
      return apply('edit', {proposal: {script: script.value, cwd: cwd.value, user: user.value, timeout_seconds: Number(timeout.value)}});
    }, fs, true);
    button('Reject request', () => apply('reject'), detail);
  }
  if (locked) {
    node('pre', r.spec.script, detail);
    button('Stop command', () => { if (confirm('Request a process-tree stop? Changes already made cannot be undone.')) apply('cancel'); }, detail).className = 'danger';
  }
  if (r.phase === 'output_held' && r.held) renderHeld(r.held);
  if (r.phase === 'ambiguous') {
    node('strong', 'Execution outcome is uncertain. Check the host before submitting another request; side effects may have occurred. This request cannot be retried here.', detail);
    node('pre', r.spec.script, detail);
  }
  if (!actionable(r)) {
    node('p', r.phase === 'released' ? 'The reviewed result has been shared with the guest. This cannot be undone.' : 'No further action is needed for this request.', detail);
    node('pre', r.spec.script, detail);
  }
}
function decode(b64) { const bytes = Uint8Array.from(atob(b64), c => c.charCodeAt(0)); return new TextDecoder().decode(bytes); }
function encode(text) { const bytes = new TextEncoder().encode(text); let binary = ''; for (const b of bytes) binary += String.fromCharCode(b); return btoa(binary); }
function renderHeld(original) {
  node('h3', 'Host-private result — NOT released', detail);
  node('strong', 'Review the WHOLE export. Executed metadata, script, cwd, environment and exit information can contain secrets too. Withhold if any non-redactable metadata is sensitive.', detail);
  const result = node('dl', undefined, detail);
  meta(result, 'Outcome', original.exit ? `${original.exit.kind}${original.exit.kind === 'exited' ? ` · exit code ${original.exit.code}` : ''}` : 'Not provided');
  meta(result, 'Capture', original.truncated ? 'Truncated — some output was not captured' : 'Complete');
  const script = field('Export executed script (replacement must be nonempty)', original.executed.script, detail, true);
  const cwd = field('Export executed cwd (clean absolute path)', original.executed.cwd, detail);
  node('p', 'Output is displayed as UTF-8 text (invalid bytes appear as replacement characters). Untouched streams preserve exact original bytes; editing explicitly replaces the entire stream with UTF-8.', detail);
  const out = field('Export stdout', decode(original.stdout), detail, true);
  const err = field('Export stderr', decode(original.stderr), detail, true);
  let outEdited = false, errEdited = false;
  out.addEventListener('input', () => { outEdited = true; }); err.addEventListener('input', () => { errEdited = true; });
  button('Preview whole export and explicit redaction markers', makePreview, detail);
  const preview = node('section', 'Preview the complete export before releasing.', detail); preview.className = 'export-preview';
  let candidate = null, confirmation = null;
  const releaseActions = node('div', undefined, detail);
  const confirmationView = node('section', undefined, detail);
  function clearConfirmation() { confirmation = null; confirmationView.replaceChildren(); }
  const release = button('Release reviewed immutable artifact to guest', () => {
    if (!candidate || stale || busy) return;
    clearConfirmation();
    // In-page confirmation keeps heartbeat/polling live. Freeze the exact
    // previewed bytes; a nonce renewal alone does not change the disclosure.
    confirmation = structuredClone(candidate);
    node('strong', 'Disclose this complete reviewed artifact, including executed metadata, to the guest? This cannot be undone.', confirmationView);
    node('p', 'The preview above is the exact content selected for release. Transport identifiers remain unchanged. Only confirmation sends it to the guest.', confirmationView);
    const frozen = confirmation;
    button('Confirm release to guest', () => {
      if (confirmation !== frozen || stale || busy) return;
      return apply('release', {artifact: structuredClone(frozen)});
    }, confirmationView);
    button('Cancel release confirmation', clearConfirmation, confirmationView);
  }, releaseActions, true);
  for (const input of [script, cwd, out, err]) input.addEventListener('input', () => { candidate = null; clearConfirmation(); release.disabled = true; preview.textContent = 'Changed: preview again.'; });
  function makePreview() {
    if (!script.value.trim() || !cwd.value.startsWith('/')) { status.textContent = 'The exported command must not be empty and its directory must be absolute.'; return; }
    clearConfirmation();
    candidate = structuredClone(original); const fields = new Set(candidate.redacted_fields);
    if (script.value !== original.executed.script) { candidate.executed.script = script.value; fields.add('executed.script'); }
    if (cwd.value !== original.executed.cwd) { candidate.executed.cwd = cwd.value; fields.add('executed.cwd'); }
    if (outEdited) { candidate.stdout = encode(out.value); fields.add('stdout'); }
    if (errEdited) { candidate.stderr = encode(err.value); fields.add('stderr'); }
    candidate.redacted_fields = [...fields];
    exportView(candidate, preview); release.disabled = false;
    status.textContent = 'Preview ready. Review all content below before releasing it to the guest.';
  }
  button('Withhold — keep all result data host-private', () => {
    candidate = null; clearConfirmation(); release.disabled = true; preview.textContent = 'Withheld. Nothing has been sent. Preview again if you decide to release later.';
    status.textContent = 'Still held. No release operation sent.';
  }, releaseActions);
}
// Only nonce/expiry are disposable offers. Epoch, revision, digest, phase,
// complete spec and held artifact still identify the state being reviewed.
function sameReview(a, b) {
  const state = r => JSON.stringify({...r, binding: {...r.binding, nonce: undefined, expires_unix: undefined}});
  return state(a) === state(b);
}
function renewOffer(next) {
  if (JSON.stringify(current.binding) !== JSON.stringify(next.binding)) {
    current = next;
    bindingView.textContent = `Revision ${next.binding.revision || 1} · Frozen by the host`;
    // Only called for the same substantive review: preserve drafts, preview
    // and in-page confirmation. Operations still use the latest exact binding;
    // expiry, epoch/state changes and single-use checks remain host-enforced.
  }
}
function selectReview(r) {
  if (selected === r.snapshot_id && current && !stale && sameReview(current, r)) renewOffer(r);
  else render(r);
  selected = r.snapshot_id;
  for (const b of document.querySelector('#list').querySelectorAll('button')) {
    const active = b.snapshotKey === r.snapshot_id;
    b.className = active ? 'request selected' : 'request';
    b.setAttribute('aria-pressed', String(active));
  }
}
async function refresh(force = false) {
  try {
    const v = await api('view'); const list = document.querySelector('#list');
    const requests = v.requests || []; latestRequests = requests;
    const signature = JSON.stringify(requests.map(r => [r.snapshot_id, r.reason, r.phase, r.spec.user]));
    if (signature !== queueSignature) {
    queueSignature = signature; list.replaceChildren();
    const pending = requests.filter(actionable);
    if (!pending.length) node('p', 'All caught up. New requests will appear here.', list).className = 'empty';
    const addRequest = (r, parent) => {
      const b = button(r.reason || 'No reason supplied', () => {
        const latest = latestRequests.find(item => item.snapshot_id === r.snapshot_id);
        if (latest) selectReview(latest);
      }, parent);
      b.snapshotKey = r.snapshot_id;
      b.className = r.snapshot_id === selected ? 'request selected' : 'request';
      b.setAttribute('aria-pressed', String(r.snapshot_id === selected));
      node('span', `${phaseNames[r.phase] || r.phase} · ${r.spec.user}`, b);
    };
    for (const r of pending) addRequest(r, list);
    const recent = requests.filter(r => !actionable(r));
    if (recent.length) {
      const history = node('details', undefined, list); node('summary', `Recent activity (${recent.length})`, history);
      for (const r of recent) addRequest(r, history);
    }
    }
    const next = requests.find(r => r.snapshot_id === selected);
    // Safe polling never overwrites unsaved edits or output redactions. Only
    // substantive changes require discarding the form via explicit selection.
    if (force && next) render(next);
    else if (current && !stale && (!next || !sameReview(next, current))) {
      stale = true;
      for (const control of detail.querySelectorAll('button,input,textarea,select')) control.disabled = true;
      status.textContent = 'This request has changed. Reopen it to review the latest host state; your draft has not been overwritten.';
      if (next) button('Review updated request', async () => {
        try { const latest = await api('view'); const request = latest.requests.find(r => r.snapshot_id === selected); if (request) selectReview(request); }
        catch (e) { status.textContent = e.message; }
      }, detail);
    } else if (next && current && !stale) renewOffer(next);
  } catch (e) { status.textContent = e.message; }
}
document.querySelector('#refresh').addEventListener('click', () => refresh());
document.querySelector('#end').addEventListener('click', async () => {
  if (!confirm('End session, revoke authentication and stop running work?')) return;
  try { await api('operation', {action: 'end', binding: {version: 0, session_id: '', snapshot_id: '', revision: 0, digest: '', nonce: '', epoch: 0, expires_unix: 0}}); auth = ''; csrf = ''; detail.replaceChildren(); status.textContent = 'Session ended.'; }
  catch (e) { status.textContent = e.message; }
});
document.addEventListener('visibilitychange', () => { if (document.visibilityState !== 'visible') leaseUntil = 0; else heartbeat(); });
(async () => {
  try { const r = await api('bootstrap', {token: bootstrap}); auth = r.auth; csrf = r.csrf; await heartbeat(); await refresh(); status.textContent = 'Connected to lapdog · Output stays private until you release it.'; }
  catch (e) { status.textContent = e.message; }
})();
setInterval(heartbeat, 3000);
setInterval(() => { if (auth) refresh(); }, 3000);
