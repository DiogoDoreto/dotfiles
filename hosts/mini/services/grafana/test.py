#!/usr/bin/env python3
"""Validate generated JSON and every query; exercise health semantics with promtool.

Usage: python3 test.py /path/to/promtool
Uses only stdlib; scratch files live in repository .private/tmp, never /tmp.
"""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

sys.dont_write_bytecode = True
import generate as g

ROOT = Path(__file__).resolve().parent
REPO = ROOT.parents[3]


def query(expr):
    return expr.replace('${host:regex}', '.*').replace('$__rate_interval', '2m').replace('$host', 'mini')


def series(metric, value, **labels):
    labels = {"job": "node", "host": "mini", **labels}
    labelstr = ','.join(f'{k}="{v}"' for k, v in labels.items())
    return {"series": metric + '{' + labelstr + '}', "values": value}


def constant(metric, value, **labels):
    return series(metric, f'{value}+0x10', **labels)


def sample(value, **labels):
    return {"labels": '{' + ','.join(f'{k}="{v}"' for k, v in sorted(labels.items())) + '}', "value": value}


def check(expr, samples):
    return {"expr": query(expr), "eval_time": "5m", "exp_samples": samples}


def main():
    snapshots = {p: p.read_bytes() for p in (ROOT / 'dashboards').glob('*.json')}
    g.build()
    assert all(p.read_bytes() == data for p, data in snapshots.items()), 'Run generate.py and stage generated dashboards'
    rules = []
    for path in sorted((ROOT / 'dashboards').glob('*.json')):
        d = json.loads(path.read_text())
        assert d['uid'] in ('host-health-fleet', 'host-health-detail')
        assert not d['editable']
        ids = [p['id'] for p in d['panels']]
        assert len(ids) == len(set(ids))
        assert d['templating']['list'][0]['query'] == 'mini,chungus,lapdog'
        for p in d['panels']:
            for t in p.get('targets', []):
                assert t['datasource'] == g.DS
                assert 'or vector(0)' not in t['expr']
                assert 'nvidia_gpu_' not in t['expr']
                assert t['instant'] == (p['type'] != 'timeseries')
                rules.append({"record": f'dashboard_query_{len(rules)}', "expr": query(t['expr'])})

    tests = []

    def case(name, inputs, checks):
        tests.append({"name": name, "interval": "30s", "input_series": inputs, "promql_expr_test": checks})

    up = constant('up', 1)
    collectors = [constant('node_scrape_collector_success', 1, collector=c) for c in ('cpu', 'meminfo', 'systemd', 'filesystem')]
    resources = [series('node_cpu_seconds_total', '0+15x10', cpu='0', mode='idle'), constant('node_memory_MemAvailable_bytes', 25), constant('node_memory_MemTotal_bytes', 100)]
    case('healthy counters and memory gauges, never-scraped lapdog remains visible', [up] + collectors + resources, [
        check(g.CPU, [sample(50, host='mini')]),
        check('sum by(host) (' + g.MEM + ')', [sample(75, host='mini')]),
        check(g.presence(), [sample(2, host='mini'), sample(0, host='chungus'), sample(0, host='lapdog')]),
        check(g.SWAP, []),
    ])
    case('failed node scrape hides cached resources', [constant('up', 0)] + collectors + resources, [
        check(g.CPU, []), check(g.MEM, []),
        check(g.presence(), [sample(1, host='mini'), sample(0, host='chungus'), sample(0, host='lapdog')]),
    ])
    case('failed collectors hide resources and service status', [up, constant('node_scrape_collector_success', 0, collector='cpu'), constant('node_scrape_collector_success', 0, collector='systemd')] + resources, [check(g.CPU, []), check(g.service_status(('mini',)), [])])

    # node_exporter exports Service.Type, not the unit category, in `type`.
    units = [
        constant('node_systemd_unit_state', 1, name='caddy.service', type='notify', state='active'),
        constant('node_systemd_unit_state', 0, name='grafana.service', type='simple', state='active'),
        constant('node_systemd_unit_state', 1, name='grafana.service', type='simple', state='failed'),
        constant('node_systemd_unit_state', 0, name='caddy.service', type='notify', state='failed'),
        constant('node_systemd_unit_state', 0, name='backup.service', type='oneshot', state='failed'),
        constant('node_systemd_unit_state', 1, name='backup.timer', type='', state='failed'),
    ]
    case('active, stopped/failed, and completely absent expected service', [up] + collectors + units, [
        check('sum by(host,name) (' + g.service_status(('mini',)) + ')', [sample(2, host='mini', name='caddy.service'), sample(1, host='mini', name='grafana.service'), sample(0, host='mini', name='sshd.service'), sample(0, host='mini', name='victoriametrics.service'), sample(0, host='mini', name='prometheus-node-exporter.service')]),
        check(g.FAILED, [sample(1, host='mini')]),
        check(g.gate(g.selector('node_systemd_unit_state', g.SERVICE_FAILED) + ' == 1', 'systemd'), [sample(1, __name__='node_systemd_unit_state', job='node', host='mini', name='grafana.service', type='simple', state='failed')]),
    ])
    case('healthy service count is observed zero; non-service failures excluded', [up] + collectors + [
        constant('node_systemd_unit_state', 0, name='caddy.service', type='notify', state='failed'),
        constant('node_systemd_unit_state', 0, name='backup.service', type='oneshot', state='failed'),
        constant('node_systemd_unit_state', 1, name='backup.timer', type='', state='failed'),
    ], [check(g.FAILED, [sample(0, host='mini')])])
    case('failed oneshot service is included', [up] + collectors + [
        constant('node_systemd_unit_state', 1, name='backup.service', type='oneshot', state='failed'),
    ], [check(g.FAILED, [sample(1, host='mini')])])
    fslabels = {"device": "/dev/test", "mountpoint": "/", "fstype": "ext4"}
    fs = [constant('node_filesystem_size_bytes', 100, **fslabels), constant('node_filesystem_avail_bytes', 20, **fslabels), constant('node_filesystem_files', 100, **fslabels), constant('node_filesystem_files_free', 30, **fslabels)]
    case('filesystem usable capacity, inode capacity, missing boot', [up] + collectors + fs + [constant('node_filesystem_device_error', 0, **fslabels)], [
        check('sum by(host) (' + g.fs_capacity() + ')', [sample(80, host='mini')]),
        check('sum by(host) (' + g.fs_capacity(True) + ')', [sample(70, host='mini')]),
        check(g.fs_status('mini'), [sample(2, host='mini', mountpoint='/'), sample(0, host='mini', mountpoint='/boot')]),
    ])
    case('device errors hide cached capacity', [up] + collectors + fs + [constant('node_filesystem_device_error', 1, **fslabels)], [check(g.fs_capacity(), []), check(g.fs_status('mini'), [sample(1, host='mini', mountpoint='/'), sample(0, host='mini', mountpoint='/boot')])])

    # Use the actual JSON expression rather than reimplementing GPU gates in test code.
    fleet = json.loads((ROOT / 'dashboards/fleet.json').read_text())
    gpu = next(p['targets'][0]['expr'] for p in fleet['panels'] if p['title'] == 'GPU utilization')
    gpu_series = [constant('up', 1, host='chungus'), constant('up', 1, host='chungus', job='nvidia_gpu'), constant('nvidia_smi_utilization_gpu_ratio', 0.25, host='chungus', job='nvidia_gpu', uuid='gpu-test')]
    case('GPU ratio scaled exactly once', gpu_series + [constant('nvidia_smi_command_exit_code', 0, host='chungus', job='nvidia_gpu')], [check('sum by(host) (' + gpu + ')', [sample(25, host='chungus')])])
    case('HTTP 200 with failed nvidia-smi is unknown', gpu_series + [constant('nvidia_smi_command_exit_code', 1, host='chungus', job='nvidia_gpu')], [check(gpu, [])])
    case('missing GPU exit code is not success', gpu_series, [check(gpu, [])])
    case('missing backend data never yields resource zeros', [], [check(g.CPU, []), check(g.MEM, []), check(g.service_status(), []), check(g.fs_capacity(), [])])

    case('removed target expires to missing, never healthy', [series('up', '1+0x4 stale _x5')], [check(g.presence(), [sample(0, host='mini'), sample(0, host='chungus'), sample(0, host='lapdog')]), check(g.CPU, [])])

    scratch = REPO / '.private/tmp'
    scratch.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='host-health-test-', dir=scratch) as tmp:
        path = Path(tmp)
        rulefile = path / 'rules.json'
        rulefile.write_text(json.dumps({"groups": [{"name": "dashboards", "rules": rules}]}))
        testfile = path / 'tests.json'
        testfile.write_text(json.dumps({"rule_files": [], "evaluation_interval": "30s", "tests": tests}))
        subprocess.run([sys.argv[1], 'check', 'rules', str(rulefile)], check=True)
        subprocess.run([sys.argv[1], 'test', 'rules', str(testfile)], check=True)
    print(f'PASS: 2 reproducible dashboards, {len(rules)} parsed queries, {len(tests)} semantic fixture scenarios')


if __name__ == '__main__':
    main()
