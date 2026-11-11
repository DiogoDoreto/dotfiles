'use strict';
// No storage, cookies, eval, HTML interpretation, guest links, or mutation GETs.
const bootstrap = location.hash.slice(1);
history.replaceState(null, '', '/');
let auth = '', csrf = '', selected = '', current = null, leaseUntil = 0, busy = false;
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
  if (busy || !current) return;
  if ((action === 'run' || action === 'run_root') && Date.now() >= leaseUntil) { status.textContent = 'Browser lease expired. Reconnect and review again.'; return; }
  busy = true;
  // Copy the exact host-provided binding, never build an approval in the browser.
  const binding = structuredClone(current.binding);
  try { await api('operation', {action, binding, ...extra}); await refresh(true); }
  catch (e) { status.textContent = e.message; }
  finally { busy = false; }
}
function render(r) {
  current = r; detail.replaceChildren();
  node('h2', `${r.phase} · ${r.snapshot_id}`, detail);
  node('h3', 'Untrusted guest reason', detail); node('pre', r.reason, detail);
  node('h3', 'Complete frozen execution specification and approval binding', detail);
  node('pre', JSON.stringify({spec: r.spec, binding: r.binding}, null, 2), detail);
  node('p', 'bash --noprofile --norc /proc/self/fd/3; immutable script FD3, never script argv; stdin /dev/null; no PTY. Environment and live cwd are shown above. Root prompts occur in KDE, never enter a password here.', detail);
  const locked = ['starting', 'running'].includes(r.phase);
  if (locked) node('strong', 'Read-only while authenticating/executing. Cancellation requests stop the process tree; they cannot undo side effects. Output stays held.', detail);
  if (['reviewing', 'approved'].includes(r.phase)) {
    const fs = node('fieldset', undefined, detail); node('legend', 'Edit then save; saving invalidates the old approval', fs);
    const script = field('Script', r.spec.script, fs, true);
    const cwd = field('Working directory', r.spec.cwd, fs);
    const timeout = field('Timeout seconds (1–1800; default 120)', r.spec.timeout_seconds || 120, fs); timeout.type = 'number'; timeout.min = '1'; timeout.max = '1800';
    const userLabel = node('label', 'Execution user', fs); const user = node('select', undefined, userLabel);
    for (const name of ['dog', 'root']) { const o = node('option', name, user); o.value = name; } user.value = r.spec.user;
    const approve = button(r.spec.user === 'root' ? 'Run as root — KDE confirmation' : 'Run as dog', () => {
      if (confirm(`Execute this exact frozen revision on lapdog as ${r.spec.user}? All output will be held.`)) apply(r.spec.user === 'root' ? 'run_root' : 'run');
    }, detail);
    if (r.spec.user === 'root') approve.className = 'danger';
    const dirty = () => { approve.disabled = true; status.textContent = 'Unsaved edit: save, then review the new host revision/digest before running.'; };
    for (const input of [script, cwd, timeout, user]) input.addEventListener('input', dirty);
    button('Save edited command (does not run)', () => apply('edit', {proposal: {script: script.value, cwd: cwd.value, user: user.value, timeout_seconds: Number(timeout.value)}}), fs);
    button('Reject request', () => apply('reject'), detail);
  }
  if (['reviewing', 'approved', 'starting', 'running'].includes(r.phase)) button('Cancel / request process-tree stop', () => apply('cancel'), detail);
  if (r.phase === 'output_held' && r.held) renderHeld(r.held);
  if (r.phase === 'ambiguous') node('strong', 'Execution outcome is uncertain. Never retry automatically; side effects may have occurred.', detail);
}
function decode(b64) { const bytes = Uint8Array.from(atob(b64), c => c.charCodeAt(0)); return new TextDecoder().decode(bytes); }
function encode(text) { const bytes = new TextEncoder().encode(text); let binary = ''; for (const b of bytes) binary += String.fromCharCode(b); return btoa(binary); }
function renderHeld(original) {
  node('h3', 'Host-private result — NOT released', detail);
  node('strong', 'Review the WHOLE export. Executed metadata, script, cwd, environment and exit information can contain secrets too. Withhold if any non-redactable metadata is sensitive.', detail);
  node('pre', JSON.stringify({...original, stdout: '(shown below; wire format is base64)', stderr: '(shown below; wire format is base64)'}, null, 2), detail);
  const script = field('Export executed script (replacement must be nonempty)', original.executed.script, detail, true);
  const cwd = field('Export executed cwd (clean absolute path)', original.executed.cwd, detail);
  node('p', 'Output is displayed as UTF-8 text (invalid bytes appear as replacement characters). Untouched streams preserve exact original bytes; editing explicitly replaces the entire stream with UTF-8.', detail);
  const out = field('Export stdout', decode(original.stdout), detail, true);
  const err = field('Export stderr', decode(original.stderr), detail, true);
  let outEdited = false, errEdited = false;
  out.addEventListener('input', () => { outEdited = true; }); err.addEventListener('input', () => { errEdited = true; });
  const preview = node('pre', 'Preview the complete export before releasing.', detail);
  let candidate = null;
  const release = button('Release reviewed immutable artifact to guest', () => {
    if (candidate && confirm('Disclose this complete reviewed artifact, including executed metadata, to the guest? This cannot be undone.')) apply('release', {artifact: candidate});
  }, detail, true);
  for (const input of [script, cwd, out, err]) input.addEventListener('input', () => { candidate = null; release.disabled = true; preview.textContent = 'Changed: preview again.'; });
  button('Preview whole export and explicit redaction markers', () => {
    candidate = structuredClone(original); const fields = new Set(candidate.redacted_fields);
    if (script.value !== original.executed.script) { candidate.executed.script = script.value; fields.add('executed.script'); }
    if (cwd.value !== original.executed.cwd) { candidate.executed.cwd = cwd.value; fields.add('executed.cwd'); }
    if (outEdited) { candidate.stdout = encode(out.value); fields.add('stdout'); }
    if (errEdited) { candidate.stderr = encode(err.value); fields.add('stderr'); }
    candidate.redacted_fields = [...fields];
    preview.textContent = JSON.stringify(candidate, null, 2); release.disabled = false;
  }, detail);
  button('Withhold — keep all result data host-private', () => { status.textContent = 'Still held. No release operation sent.'; }, detail);
}
async function refresh(force = false) {
  try {
    const v = await api('view'); const list = document.querySelector('#list'); list.replaceChildren();
    for (const r of v.requests || []) button(`${r.phase}: ${r.reason || r.snapshot_id}`, () => { selected = r.snapshot_id; render(r); }, list);
    const next = (v.requests || []).find(r => r.snapshot_id === selected);
    // Safe polling never overwrites unsaved edits or output redactions. If host
    // state changed, disable stale forms and require explicit fresh selection.
    if (force && next) render(next);
    else if (next && current && JSON.stringify(next) !== JSON.stringify(current)) {
      for (const control of detail.querySelectorAll('button,input,textarea,select')) control.disabled = true;
      status.textContent = 'Host state changed. Select the request again and review the new state.';
    }
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
  try { const r = await api('bootstrap', {token: bootstrap}); auth = r.auth; csrf = r.csrf; await heartbeat(); await refresh(); status.textContent = 'Authenticated local reviewer. Output always held until explicit release.'; }
  catch (e) { status.textContent = e.message; }
})();
setInterval(heartbeat, 3000);
setInterval(() => { if (auth) refresh(); }, 3000);
