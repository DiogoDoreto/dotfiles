// Dependency-free DOM behavior harness; not a substitute for browser automation.
const {test} = require('node:test');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
class Element {
  constructor(tag) { this.tag = tag; this.children = []; this.events = {}; this.textContent = ''; this.value = ''; this.disabled = false; }
  append(n) { this.children.push(n); }
  replaceChildren(...nodes) { this.children = nodes; }
  addEventListener(event, fn) { (this.events[event] ||= []).push(fn); }
  querySelectorAll() { return this.children.flatMap(n => [n, ...n.querySelectorAll()]); }
  async emit(event) { for (const fn of this.events[event] || []) await fn(); }
}
function harness() {
  const elements = Object.fromEntries(['status','detail','list','refresh','end'].map(id => [`#${id}`,new Element('div')]));
  const calls = [], history = [];
  const context = vm.createContext({
    document: {querySelector: s => elements[s], createElement: tag => new Element(tag), addEventListener() {}, visibilityState:'visible'},
    location: {hash:'#fragment-secret'}, history: {replaceState: (...args) => history.push(args)},
    fetch: async (url, options) => {
      calls.push({url, options});
      return {ok:true,json:async () => url.endsWith('bootstrap') ? {auth:'auth-secret',csrf:'csrf-secret'} : url.endsWith('heartbeat') ? {lease_seconds:10} : {requests:[]}};
    }, structuredClone, TextEncoder, TextDecoder, atob, btoa, confirm:()=>true, setInterval() {}, Date, JSON, Uint8Array,
  });
  vm.runInContext(fs.readFileSync(path.join(__dirname,'assets/app.js'),'utf8'), context);
  return {context,elements,calls,history,run:code=>vm.runInContext(code,context)};
}
const tick = () => new Promise(resolve => setImmediate(resolve));
function all(e) { return e.querySelectorAll(); }
function button(e,label) { const b = all(e).find(n => n.tag==='button' && n.textContent.startsWith(label)); assert.ok(b,label); return b; }
function input(e,label) { const l = all(e).find(n=>n.tag==='label' && n.textContent===label); assert.ok(l,label); return l.children[0]; }

test('fragment removed, credentials kept in memory, polling GET only', async () => {
  const h = harness(); await tick();
  assert.equal(h.history[0][2],'/');
  assert.equal(h.calls[0].url,'/api/bootstrap');
  assert.deepEqual(JSON.parse(h.calls[0].options.body),{token:'fragment-secret'});
  assert.equal(h.calls.at(-1).options.method,'GET');
  assert.equal(h.calls.at(-1).options.credentials,'omit');
  assert.equal(h.calls.at(-1).options.headers.Authorization,'Bearer auth-secret');
  assert.ok(h.calls.every(c=>!c.url.includes('secret')));
});
test('hostile reason/script are plain text; unsaved edit disables approval', async () => {
  const h=harness();await tick();
  h.run(`render({snapshot_id:'id',phase:'reviewing',reason:'<img src=x onerror=alert(1)>',spec:{script:'<script>alert(1)</script>',cwd:'/home/dog',user:'dog',timeout_seconds:120},binding:{nonce:'original'}})`);
  const detail=h.elements['#detail'];
  assert.ok(all(detail).some(n=>n.tag==='pre' && n.textContent==='<img src=x onerror=alert(1)>'));
  assert.equal(input(detail,'Script').value,'<script>alert(1)</script>');
  const run=button(detail,'Run as dog');assert.equal(run.disabled,false);
  input(detail,'Script').value='changed';await input(detail,'Script').emit('input');assert.equal(run.disabled,true);
});
test('authentication interval is read only except explicit cancellation', async()=>{
  const h=harness();await tick();h.run(`render({snapshot_id:'id',phase:'starting',reason:'root',spec:{user:'root'},binding:{}})`);
  const d=h.elements['#detail'];assert.equal(all(d).filter(n=>n.tag==='textarea'||n.tag==='input'||n.tag==='select').length,0);
  assert.deepEqual(all(d).filter(n=>n.tag==='button').map(n=>n.textContent),['Cancel / request process-tree stop']);
});
test('held result requires whole-export preview; edits are marked; withhold sends nothing',async()=>{
  const h=harness();await tick();
  h.run(`render({snapshot_id:'id',phase:'output_held',reason:'',spec:{user:'dog'},binding:{nonce:'original'},held:{executed:{script:'secret command',cwd:'/secret'},stdout:'c2VjcmV0',stderr:'',redacted_fields:[],execution_digest:'original-digest'}})`);
  const d=h.elements['#detail'];const release=button(d,'Release reviewed');assert.equal(release.disabled,true);
  const before=h.calls.length;await button(d,'Withhold').emit('click');assert.equal(h.calls.length,before);
  const script=input(d,'Export executed script (replacement must be nonempty)');script.value='[REDACTED]';await script.emit('input');
  const out=input(d,'Export stdout');out.value='safe';await out.emit('input');
  await button(d,'Preview whole').emit('click');assert.equal(release.disabled,false);
  await release.emit('click');
  const op=JSON.parse(h.calls.find(c=>c.url.endsWith('/operation')).options.body);
  assert.equal(op.action,'release');assert.equal(op.binding.nonce,'original');
  assert.equal(op.artifact.execution_digest,'original-digest');
  assert.equal(op.artifact.executed.script,'[REDACTED]');assert.equal(op.artifact.stdout,btoa('safe'));
  assert.deepEqual(op.artifact.redacted_fields,['executed.script','stdout']);
});
