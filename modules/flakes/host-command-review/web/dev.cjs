// Development only. No command execution, real capabilities, storage, or backend imports.
// Run: node web/dev.cjs [port]. Restart to reset all fixtures.
const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');
const spec = {version:1, script:'systemctl --failed --no-pager\njournalctl -u backup.service -n 40 --no-pager', cwd:'/home/dog', user:'dog', shell:'/bin/bash', environment:{PATH:'/run/current-system/sw/bin',HOME:'/home/dog',LANG:'en_US.UTF-8'}, timeout_seconds:120, stop_grace_seconds:5, capture_bytes:1048576};
const binding = i => ({version:1,session_id:'demo',snapshot_id:`demo-${i}`,revision:1,digest:'mock-only',epoch:1,nonce:'mock-only',expires_unix:Math.floor(Date.now()/1000)+300});
const request = (i,reason,phase,changes={}) => ({snapshot_id:`demo-${i}`,reason,phase,spec:{...structuredClone(spec),...changes},binding:binding(i)});
const requests = [
  request(1,'Find out why the nightly backup failed','reviewing'),
  request(2,'Check disk usage before cleaning old build files','output_held',{script:'df -h /\ndu -sh /nix/store'}),
  request(3,'Restart the backup service after updating its configuration','reviewing',{user:'root',script:'systemctl restart backup.service'}),
  request(4,'Verify the working tree is clean','released',{script:'git status --short'}),
  request(5,'Investigate an interrupted database migration','ambiguous',{script:'./migrate.sh'}),
];
function held(r) { return {version:1,artifact_id:'mock-artifact',snapshot_id:r.snapshot_id,revision:r.binding.revision,execution_digest:'mock-only',executed:structuredClone(r.spec),stdout:Buffer.from('Filesystem      Size  Used Avail Use% Mounted on\n/dev/nvme0n1p2  920G  712G  161G  82% /\n384G\t/nix/store\n').toString('base64'),stderr:Buffer.from('Note: private host path /home/dog/backups is not included.\n').toString('base64'),exit:{kind:'exited',code:0,signal:0},truncated:false,redacted_fields:[]}; }
requests[1].held=held(requests[1]);
let ended=false, generation=1;
const assets = {'/':['index.html','text/html'],'/app.js':['app.js','text/javascript'],'/style.css':['style.css','text/css']};
const server=http.createServer(async(req,res)=>{
  res.setHeader('Cache-Control','no-store');
  res.setHeader('Content-Security-Policy',"default-src 'none'; script-src 'self'; style-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'");
  if (!req.url.startsWith('/api/')) {
    const asset=assets[req.url]; if(!asset){res.writeHead(404).end();return;}
    res.setHeader('Content-Type',asset[1]);
    const content=fs.readFileSync(path.join(__dirname,'assets',asset[0]));
    res.end(req.url==='/' ? content.toString().replace('<body>', '<body><div class="demo-banner">DESIGN SANDBOX · Mock data only · No commands execute · Restart to reset</div>') : content);return;
  }
  res.setHeader('Content-Type','application/json');
  try {
    let raw='';for await(const chunk of req){raw+=chunk;if(raw.length>9*1024*1024)throw Error();}
    const body=raw?JSON.parse(raw):{};
    if(req.url==='/api/bootstrap')return res.end(JSON.stringify({auth:'a'.repeat(64),csrf:'b'.repeat(64),expires_unix:Math.floor(Date.now()/1000)+14400}));
 if(req.url==='/api/claim')return res.end('{}');
    if(req.url==='/api/heartbeat')return res.end(JSON.stringify({lease_seconds:10}));
    if(req.url==='/api/view'){
      for(const r of requests)r.binding.expires_unix=Math.floor(Date.now()/1000)+300;
      return res.end(JSON.stringify({session_id:ended?'':`demo-${generation}`,state:ended?'idle':'active',routing_code:'DEMO-CODE',expires_unix:Math.floor(Date.now()/1000)+14400,transport:ended?'closed':'mock connected',requests:ended?[]:requests}));
    }
    if(req.url!=='/api/operation'||req.method!=='POST')throw Error();
    if(['end','quit','start','restart'].includes(body.action)){
 if(['end','quit','restart'].includes(body.action)){ended=true;requests.splice(0);}
 if(['start','restart'].includes(body.action)){ended=false;generation++;}
 return res.end('{}');
 }
 if(ended)throw Error();
    const r=requests.find(r=>r.snapshot_id===body.binding.snapshot_id);if(!r)throw Error();
    if(body.action==='edit'){Object.assign(r.spec,body.proposal);r.binding.revision++;}
    else if(['run','run_root'].includes(body.action)){
      r.phase='starting';setTimeout(()=>{if(r.phase==='starting')r.phase='running';},1500);
      setTimeout(()=>{if(r.phase==='running'){r.phase='output_held';r.held=held(r);}},4500);
    }else if(body.action==='release'){r.phase='released';delete r.held;}
    else if(body.action==='reject')r.phase='rejected';
    else if(body.action==='cancel')r.phase='cancelled';
    else throw Error();
    res.end('{}');
  } catch {res.writeHead(409).end('{}');}
});
server.listen(Number(process.argv[2]||4173),'127.0.0.1',()=>console.log(`MOCK ONLY — no commands execute. Open http://127.0.0.1:${server.address().port}/#demo (restart to reset)`));
