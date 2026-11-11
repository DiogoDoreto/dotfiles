// Dependency-free DOM behavior harness; not a substitute for browser automation.
const {test} = require('node:test');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
class Element {
  constructor(tag) { this.tag = tag; this.children = []; this.events = {}; this.textContent = ''; this.value = ''; this.disabled = false; }
  append(n) { this.children.push(n); }
  setAttribute(name, value) { this[name] = value; }
  replaceChildren(...nodes) { this.children = nodes; }
  addEventListener(event, fn) { (this.events[event] ||= []).push(fn); }
  querySelectorAll() { return this.children.flatMap(n => [n, ...n.querySelectorAll()]); }
  async emit(event) { for (const fn of this.events[event] || []) await fn(); }
}
function harness() {
  const elements = Object.fromEntries(['status','detail','list','refresh','end'].map(id => [`#${id}`,new Element('div')]));
  const calls = [], history = [];
  const view = {requests:[]};
  let now = Date.now();
  class Clock extends Date { static now() { return now; } }
  const context = vm.createContext({
    document: {querySelector: s => elements[s], createElement: tag => new Element(tag), addEventListener() {}, visibilityState:'visible'},
    location: {hash:'#fragment-secret'}, history: {replaceState: (...args) => history.push(args)},
    fetch: async (url, options) => {
      calls.push({url, options});
      return {ok:true,json:async () => url.endsWith('bootstrap') ? {auth:'auth-secret',csrf:'csrf-secret'} : url.endsWith('heartbeat') ? {lease_seconds:10} : structuredClone(view)};
    }, structuredClone, TextEncoder, TextDecoder, atob, btoa, confirm:()=>true, setInterval() {}, Date: Clock, JSON, Uint8Array,
  });
  vm.runInContext(fs.readFileSync(path.join(__dirname,'assets/app.js'),'utf8'), context);
  return {context,elements,calls,history,view,advance:ms=>{now+=ms;},now:()=>now,run:code=>vm.runInContext(code,context)};
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
  assert.ok(all(detail).some(n=>n.tag==='h2' && n.textContent==='<img src=x onerror=alert(1)>'));
  assert.equal(input(detail,'Script').value,'<script>alert(1)</script>');
  const run=button(detail,'Run as dog');assert.equal(run.disabled,false);
  input(detail,'Script').value='changed';await input(detail,'Script').emit('input');assert.equal(run.disabled,true);
});
test('authentication interval is read only except explicit cancellation', async()=>{
  const h=harness();await tick();h.run(`render({snapshot_id:'id',phase:'starting',reason:'root',spec:{user:'root'},binding:{}})`);
  const d=h.elements['#detail'];assert.equal(all(d).filter(n=>n.tag==='textarea'||n.tag==='input'||n.tag==='select').length,0);
  assert.deepEqual(all(d).filter(n=>n.tag==='button').map(n=>n.textContent),['Stop command']);
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
  await button(d,'Confirm release to guest').emit('click');
  const op=JSON.parse(h.calls.find(c=>c.url.endsWith('/operation')).options.body);
  assert.equal(op.action,'release');assert.equal(op.binding.nonce,'original');
  assert.equal(op.artifact.execution_digest,'original-digest');
  assert.equal(op.artifact.executed.script,'[REDACTED]');assert.equal(op.artifact.stdout,btoa('safe'));
  assert.deepEqual(op.artifact.redacted_fields,['executed.script','stdout']);
});

test('queue is reason-led, hides completed work and preserves focusable nodes across polling', async()=>{
  const h=harness();await tick();
  const active=review(), completed={...review('released'),snapshot_id:'done',reason:'Completed task'};
  h.view.requests=[active,completed];await h.run('refresh()');
  const list=h.elements['#list'];const item=button(list,'guest');
  assert.ok(all(list).some(n=>n.tag==='details'));
  assert.ok(!all(list).some(n=>n.textContent.includes('digest')));
  await h.run('refresh()');assert.equal(button(list,'guest'),item);
  // A preserved button must select the latest state, not its creation-time closure.
  h.view.requests[0].spec.script='latest command';await h.run('refresh()');await item.emit('click');
  assert.equal(input(h.elements['#detail'],'Script').value,'latest command');
});

test('readable export preview contains content and policy but no raw JSON or identity hashes', async()=>{
  const h=harness();await tick();await select(h,review('output_held'));
  const d=h.elements['#detail'];await button(d,'Preview whole').emit('click');
  const text=all(d).map(n=>n.textContent).join('\n');
  assert.ok(text.includes('What the guest will receive'));
  assert.ok(text.includes('secret'));assert.ok(text.includes('Directory'));
  assert.ok(!text.includes('execution_digest'));assert.ok(!text.includes('"stdout":'));
  await button(d,'Release reviewed').emit('click');await button(d,'Withhold').emit('click');
  assert.equal(button(d,'Release reviewed').disabled,true);
  assert.ok(!all(d).some(n=>n.tag==='button'&&n.textContent==='Confirm release to guest'));
});

function review(phase='reviewing') {
  return {snapshot_id:'id',phase,reason:'guest',spec:{script:'original',cwd:'/home/dog',user:'dog',timeout_seconds:120},
    binding:{version:1,session_id:'session',snapshot_id:'id',revision:1,digest:'digest',epoch:1,nonce:'old',expires_unix:Math.floor(Date.now()/1000)+30},
    ...(phase==='output_held' ? {held:{executed:{script:'secret',cwd:'/secret'},stdout:'/w==',stderr:'c2VjcmV0',redacted_fields:[],execution_digest:'digest'}} : {})};
}
async function select(h,r) {
  h.view.requests=[r]; await h.run('refresh()'); await button(h.elements['#list'],r.reason).emit('click');
}
async function rotate(h) {
  h.view.requests[0].binding.nonce='renewed';
  h.view.requests[0].binding.expires_unix=Math.floor(h.now()/1000)+30;
  await h.run('refresh()');
}
function lastOperation(h) { return JSON.parse(h.calls.filter(c=>c.url.endsWith('/operation')).at(-1).options.body); }

test('polling binding expiry/rotation preserves command drafts, Save and dirty run interlock',async()=>{
  const h=harness(); await tick(); await select(h,review()); const d=h.elements['#detail'];
  const script=input(d,'Script');script.value='unfinished draft';await script.emit('input');
  // Cross the actual 30-second expiry while a draft is still unfinished.
  h.advance(31000);
  await h.run('refresh()'); assert.equal(button(d,'Save edited').disabled,false);
  await rotate(h);
  assert.equal(input(d,'Script'),script);assert.equal(script.value,'unfinished draft');
  assert.equal(button(d,'Run as dog').disabled,true);assert.equal(button(d,'Save edited').disabled,false);
  await button(h.elements['#list'],'guest').emit('click');assert.equal(input(d,'Script'),script);
  await button(d,'Save edited').emit('click');
  const op=lastOperation(h);assert.equal(op.binding.nonce,'renewed');assert.equal(op.proposal.script,'unfinished draft');assert.equal(op.action,'edit');
});

test('expired run sends nothing; rotated unchanged command requires explicit run confirmation',async()=>{
  const h=harness();await tick();await select(h,review());const d=h.elements['#detail'];
  h.advance(31000);
  let confirmations=0;h.context.confirm=()=>{confirmations++;return true;};
  await button(d,'Run as dog').emit('click'); assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  await rotate(h);await h.run('heartbeat()');assert.equal(button(d,'Run as dog').disabled,false);
  h.context.confirm=()=>{confirmations++;return false;};await button(d,'Run as dog').emit('click');
  assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  h.context.confirm=()=>{confirmations++;return true;};await button(d,'Run as dog').emit('click');
  assert.equal(lastOperation(h).binding.nonce,'renewed');assert.equal(lastOperation(h).action,'run');assert.ok(confirmations>=2);
});

test('held redactions and preview survive offer renewal; release requires explicit confirmation',async()=>{
  const h=harness();await tick();await select(h,review('output_held'));const d=h.elements['#detail'];
  const err=input(d,'Export stderr');err.value='safe';await err.emit('input');
  const script=input(d,'Export executed script (replacement must be nonempty)');script.value='redacted';await script.emit('input');
  await button(d,'Preview whole').emit('click');const release=button(d,'Release reviewed');assert.equal(release.disabled,false);
  h.advance(31000);
  await h.run('refresh()');assert.equal(err.value,'safe');
  await rotate(h);assert.equal(input(d,'Export stderr'),err);assert.equal(err.value,'safe');assert.equal(script.value,'redacted');
  assert.equal(release.disabled,false);assert.equal(button(d,'Preview whole').disabled,false);
  await button(h.elements['#list'],'guest').emit('click');assert.equal(input(d,'Export stderr'),err);
  await release.emit('click');assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  await button(d,'Cancel release confirmation').emit('click');
  await release.emit('click');await button(d,'Confirm release to guest').emit('click');const op=lastOperation(h);
  assert.equal(op.binding.nonce,'renewed');assert.equal(op.artifact.stderr,btoa('safe'));assert.equal(op.artifact.stdout,'/w==');
  assert.deepEqual(op.artifact.redacted_fields,['executed.script','stderr']);
});

test('release confirmation is non-modal, survives renewal and keeps redactions and heartbeat',async()=>{
  const h=harness();await tick();await select(h,review('output_held'));const d=h.elements['#detail'];
  h.context.confirm=()=>{throw new Error('release must not block browser heartbeats with a modal');};
  const out=input(d,'Export stdout');out.value='[REDACTED]';await out.emit('input');
  await button(d,'Preview whole').emit('click');await button(d,'Release reviewed').emit('click');
  const confirm=button(d,'Confirm release to guest');
  assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  h.advance(31000);await rotate(h);await h.run('heartbeat()');
  assert.equal(button(d,'Confirm release to guest'),confirm);assert.equal(out.value,'[REDACTED]');
  await confirm.emit('click');const op=lastOperation(h);
  assert.equal(op.binding.nonce,'renewed');assert.equal(op.artifact.stdout,btoa('[REDACTED]'));
  assert.deepEqual(op.artifact.redacted_fields,['stdout']);
});

test('editing after opening release confirmation invalidates that confirmation',async()=>{
  const h=harness();await tick();await select(h,review('output_held'));const d=h.elements['#detail'];
  await button(d,'Preview whole').emit('click');await button(d,'Release reviewed').emit('click');
  const confirm=button(d,'Confirm release to guest');const out=input(d,'Export stdout');
  out.value='new draft';await out.emit('input');await confirm.emit('click');
  assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  assert.equal(button(d,'Release reviewed').disabled,true);
});

test('authority or held-artifact changes block an already-open release confirmation',async()=>{
  for(const change of [r=>r.binding.epoch++,r=>r.binding.revision++,r=>r.held.stderr=btoa('changed')]) {
    const h=harness();await tick();await select(h,review('output_held'));const d=h.elements['#detail'];
    const out=input(d,'Export stdout');out.value='safe';await out.emit('input');
    await button(d,'Preview whole').emit('click');await button(d,'Release reviewed').emit('click');
    const confirm=button(d,'Confirm release to guest');
    change(h.view.requests[0]);await rotate(h);
    assert.equal(confirm.disabled,true);assert.equal(out.value,'safe');
    await confirm.emit('click');assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  }
});

test('substantive command/authority changes invalidate stale forms instead of adopting bindings',async()=>{
  for(const change of [r=>r.binding.revision++,r=>r.binding.epoch++,r=>r.binding.digest='different',r=>r.spec.script='new command',r=>r.phase='starting']) {
    const h=harness();await tick();await select(h,review());const d=h.elements['#detail'];
    input(d,'Script').value='draft';await input(d,'Script').emit('input');
    change(h.view.requests[0]);await rotate(h);
    assert.equal(input(d,'Script').value,'draft');assert.equal(button(d,'Run as dog').disabled,true);assert.equal(button(d,'Save edited').disabled,true);
    // Even programmatic invocation must not send a stale operation.
    await h.run(`apply('run')`);assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  }
});

test('real held artifact changes invalidate preview/release and require explicit fresh review',async()=>{
  const h=harness();await tick();await select(h,review('output_held'));const d=h.elements['#detail'];
  const out=input(d,'Export stdout');out.value='draft';await out.emit('input');await button(d,'Preview whole').emit('click');
  h.view.requests[0].held.stderr=btoa('new private data');await rotate(h);
  assert.equal(out.value,'draft');assert.equal(button(d,'Release reviewed').disabled,true);assert.equal(button(d,'Preview whole').disabled,true);
  await h.run(`apply('release', {artifact:{}})`);assert.ok(!h.calls.some(c=>c.url.endsWith('/operation')));
  await button(h.elements['#list'],'guest').emit('click');
  assert.equal(input(d,'Export stderr').value,'new private data');assert.equal(button(d,'Release reviewed').disabled,true);
});
