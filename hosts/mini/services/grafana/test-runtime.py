#!/usr/bin/env python3
"""Exercise the provisioned datasource through Grafana, not just VictoriaMetrics.

Usage: test-runtime.py GRAFANA_PACKAGE EVALUATED_CONFIG [PLUGIN_DIRECTORY]
Uses a disposable loopback metrics stub, never production state or credentials.
Pass the evaluated settings.paths.plugins as PLUGIN_DIRECTORY to test Nix plugins.
"""
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.error import HTTPError
from urllib.request import Request, urlopen


class Metrics(BaseHTTPRequestHandler):
    def do_GET(self):
        body = json.dumps({"status": "success", "data": {
            "resultType": "vector", "result": [
                {"metric": {}, "value": [time.time(), "42"]}
            ]}}).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        self.rfile.read(int(self.headers.get('Content-Length', 0)))
        self.do_GET()

    def log_message(self, *args):
        pass


def main():
    package, config = map(lambda p: Path(p).resolve(), sys.argv[1:3])
    scratch = Path(__file__).resolve().parents[4] / '.private/tmp'
    scratch.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='grafana-query-', dir=scratch) as tmp:
        root = Path(tmp)
        provisioning = root / 'provisioning/datasources'
        provisioning.mkdir(parents=True)
        with ThreadingHTTPServer(('127.0.0.1', 0), Metrics) as metrics:
            threading.Thread(target=metrics.serve_forever, daemon=True).start()
            datasource = {'name': 'VictoriaMetrics', 'uid': 'victoriametrics',
                          'type': 'prometheus', 'access': 'proxy',
                          'url': f'http://127.0.0.1:{metrics.server_port}'}
            (provisioning / 'datasource.yaml').write_text(json.dumps({
                'apiVersion': 1, 'datasources': [datasource]}))
            key = root / 'key'
            key.write_text(os.urandom(32).hex())
            key.chmod(0o600)
            # Match the NixOS service's writable home with a conf symlink.
            (root / 'conf').symlink_to(package / 'share/grafana/conf')
            with socket.socket() as sock:
                sock.bind(('127.0.0.1', 0))
                port = sock.getsockname()[1]
            plugins = str(Path(sys.argv[3]).resolve()) if len(sys.argv) > 3 else str(root / 'plugins')
            command = [str(package / 'bin/grafana'), 'server', '--homepath', str(root),
                       '--config', str(config), f'cfg:server.http_port={port}',
                       f'cfg:paths.data={root}/data', f'cfg:paths.logs={root}/logs',
                       f'cfg:paths.plugins={plugins}',
                       f'cfg:paths.provisioning={root}/provisioning',
                       f'cfg:database.path={root}/grafana.db',
                       f'cfg:security.secret_key=$__file{{{key}}}']

            def request(path, payload=None):
                headers = {'X-WEBAUTH-USER': 'fixture-viewer'}
                if payload is not None:
                    headers['Content-Type'] = 'application/json'
                req = Request(f'http://127.0.0.1:{port}' + path,
                              data=json.dumps(payload).encode() if payload else None,
                              headers=headers)
                try:
                    with urlopen(req, timeout=10) as response:
                        return response.status, json.load(response)
                except HTTPError as exc:
                    return exc.code, exc.read().decode()

            with (root / 'grafana.log').open('w') as log:
                proc = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT)
                try:
                    for _ in range(120):
                        try:
                            if request('/api/health')[0] == 200:
                                break
                        except OSError:
                            pass
                        assert proc.poll() is None, 'Grafana exited before health check'
                        time.sleep(0.25)
                    else:
                        raise AssertionError('Grafana startup timeout')
                    status, result = request('/api/plugins/prometheus/settings')
                    assert status == 200, ('Prometheus plugin unavailable', status, result)
                    now = int(time.time() * 1000)
                    status, result = request('/api/ds/query', {
                        'from': str(now - 60000), 'to': str(now), 'queries': [{
                            'refId': 'A', 'datasource': {'uid': 'victoriametrics', 'type': 'prometheus'},
                            'expr': 'vector(42)', 'instant': True, 'range': False,
                            'intervalMs': 30000, 'maxDataPoints': 100}]})
                    assert status == 200, ('Grafana datasource query failed', status, result)
                    answer = result['results']['A']
                    assert not answer.get('error'), answer
                    values = [value for frame in answer.get('frames', [])
                              for column in frame.get('data', {}).get('values', [])
                              for value in column]
                    assert 42 in values, ('Panel query returned no numeric sample', result)
                    print('PASS: Grafana loads Prometheus plugin and returns numeric panel data')
                except Exception:
                    print((root / 'grafana.log').read_text()[-8000:], file=sys.stderr)
                    raise
                finally:
                    proc.terminate()
                    try:
                        proc.wait(timeout=15)
                    except subprocess.TimeoutExpired:
                        proc.kill()
                        proc.wait()
                    metrics.shutdown()


if __name__ == '__main__':
    main()
