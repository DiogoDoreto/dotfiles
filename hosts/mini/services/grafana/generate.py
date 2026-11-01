#!/usr/bin/env python3
"""Generate reviewable provisioned JSON. Run from any directory; no dependencies."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent
DS = {"type": "prometheus", "uid": "victoriametrics"}
HOSTS = ("mini", "chungus", "lapdog")
SERVICES = {
    "mini": ("caddy", "victoriametrics", "grafana", "prometheus-node-exporter", "sshd"),
    "chungus": ("NetworkManager", "sshd", "prometheus-node-exporter", "prometheus-nvidia-gpu-exporter"),
    "lapdog": ("NetworkManager", "sshd", "prometheus-node-exporter"),
}
MOUNTS = {"mini": ("/", "/boot"), "chungus": ("/", "/boot", "/data", "/mnt/snapraid-parity1"), "lapdog": ("/", "/boot")}
H = '${host:regex}'


def selector(metric, extra="", host=H, job="node"):
    return f'{metric}{{job="{job}",host=~"{host}"{"," + extra if extra else ""}}}'


def inventory(labels):
    expr = "vector(0)"
    for key, value in labels.items():
        expr = f'label_replace({expr}, "{key}", "{value}", "", "")'
    return expr


def expected(hosts=HOSTS):
    return "(" + " or ".join(inventory({"host": h}) for h in hosts) + ")"


def gate(expr, collector=None, host=H):
    out = f'({expr}) and on(host) (max by(host) ({selector("up", host=host)}) == 1)'
    if collector:
        out += f' and on(host) (max by(host) ({selector("node_scrape_collector_success", f"collector=\"{collector}\"", host)}) == 1)'
    return out


def presence(hosts=HOSTS, host=H):
    up = selector("up", host=host)
    return f'(max by(host) ({up}) or on(host) {expected(hosts)}) + on(host) (count by(host) ({up}) or on(host) {expected(hosts)})'


def service_status(hosts=HOSTS):
    panels = []
    for h in hosts:
        for name in SERVICES[h]:
            labels = {"host": h, "name": name + ".service"}
            active = selector("node_systemd_unit_state", f'name="{name}.service",state="active"', h)
            e = inventory(labels)
            # 0 absent, 1 present but not active, 2 active. Scrape/collector failure => unknown.
            expr = gate(f'({active} or on(host,name) {e}) + on(host,name) (count by(host,name) ({active}) or on(host,name) {e})', "systemd", h)
            panels.append(expr)
    return " or ".join(f"({x})" for x in panels)


def fs_selector(metric, extra="", host=H):
    return selector(metric, 'fstype=~"ext4|btrfs|xfs|vfat",mountpoint=~"/|/boot|/data|/mnt/snapraid-parity1"' + ("," + extra if extra else ""), host)


def fs_capacity(inodes=False):
    total = fs_selector("node_filesystem_files" if inodes else "node_filesystem_size_bytes")
    avail = fs_selector("node_filesystem_files_free" if inodes else "node_filesystem_avail_bytes")
    return gate(f'100 * (1 - {avail} / {total}) and ({total} > 0) and ({fs_selector("node_filesystem_device_error")} == 0)', "filesystem")


def fs_status(h):
    result = []
    for mount in MOUNTS[h]:
        e = inventory({"host": h, "mountpoint": mount})
        err = selector("node_filesystem_device_error", f'mountpoint="{mount}"', h)
        expr = f'((1 - max by(host,mountpoint) ({err})) + on(host,mountpoint) count by(host,mountpoint) ({err})) or on(host,mountpoint) {e}'
        result.append(gate(expr, "filesystem", h))
    return " or ".join(f"({x})" for x in result)


STATUS = [(0, "Missing", "red"), (1, "Scrape failed", "orange"), (2, "Reachable", "green")]
SERVICE_STATUS = [(0, "Missing unit", "red"), (1, "Not active", "orange"), (2, "Active", "green")]
FS_STATUS = [(0, "Missing mount", "red"), (1, "Device error", "red"), (2, "Available", "green")]


class Dashboard:
    def __init__(self, uid, title, detail=False):
        self.y = 0
        self.data = {
            "uid": uid, "title": title, "schemaVersion": 39, "version": 1,
            "editable": False, "tags": ["host-health", "provisioned"], "timezone": "browser",
            "refresh": "30s", "time": {"from": "now-6h", "to": "now"},
            "graphTooltip": 1, "panels": [],
            "links": [{"title": "Fleet overview" if detail else "Host detail", "type": "link", "url": "/d/host-health-fleet" if detail else "/d/host-health-detail?var-host=mini", "includeVars": False, "keepTime": True}],
            "templating": {"list": [{"name": "host", "label": "Host", "type": "custom", "query": ",".join(HOSTS), "multi": not detail, "includeAll": not detail, "allValue": ".*" if not detail else None, "current": {"text": "mini" if detail else "All", "value": "mini" if detail else "$__all"}, "options": [{"text": h, "value": h, "selected": h == "mini" and detail} for h in HOSTS]}]},
        }

    def row(self, title):
        self.data["panels"].append({"id": len(self.data["panels"]) + 1, "type": "row", "title": title, "collapsed": False, "gridPos": {"x": 0, "y": self.y, "w": 24, "h": 1}})
        self.y += 1

    def panel(self, title, expr, unit="short", description="", kind="timeseries", legend="{{host}}", mappings=None, x=0, w=12, height=8, advance=False, thresholds=None):
        defaults = {"unit": unit, "noValue": "Unknown", "color": {"mode": "palette-classic"}, "custom": {"drawStyle": "line", "lineWidth": 2, "fillOpacity": 8, "spanNulls": False}}
        defaults["links"] = [{"title": "Host detail", "url": "/d/host-health-detail?var-host=${__field.labels.host}&${__url_time_range}"}]
        if unit == "percent":
            defaults.update({"min": 0, "max": 100})
        if thresholds:
            defaults["thresholds"] = {"mode": "absolute", "steps": [{"color": c, "value": v} for v, c in thresholds]}
            defaults["color"] = {"mode": "thresholds"}
        if mappings:
            defaults["mappings"] = [{"type": "value", "options": {str(v): {"text": t, "color": c, "index": i} for i, (v, t, c) in enumerate(mappings)}}]
            defaults["color"] = {"mode": "thresholds"}
        targets = expr if isinstance(expr, list) else [(expr, legend)]
        p = {"id": len(self.data["panels"]) + 1, "title": title, "description": description, "type": kind, "datasource": DS, "gridPos": {"x": x, "y": self.y, "w": w, "h": height}, "fieldConfig": {"defaults": defaults, "overrides": []}, "targets": [{"refId": chr(65 + i), "expr": e, "legendFormat": l, "range": kind == "timeseries", "instant": kind != "timeseries", "datasource": DS} for i, (e, l) in enumerate(targets)]}
        if kind == "stat":
            p["options"] = {"reduceOptions": {"calcs": ["lastNotNull"], "values": False}, "textMode": "value_and_name", "colorMode": "value", "graphMode": "none", "orientation": "horizontal"}
        else:
            p["options"] = {"legend": {"displayMode": "table", "placement": "bottom", "calcs": ["last", "max"]}, "tooltip": {"mode": "multi", "sort": "desc"}}
        self.data["panels"].append(p)
        if advance:
            self.y += height

    def text(self, title, text):
        self.data["panels"].append({"id": len(self.data["panels"]) + 1, "title": title, "type": "text", "gridPos": {"x": 0, "y": self.y, "w": 24, "h": 4}, "options": {"mode": "markdown", "content": text}})
        self.y += 4

    def save(self, name):
        (ROOT / "dashboards").mkdir(exist_ok=True)
        (ROOT / "dashboards" / name).write_text(json.dumps(self.data, indent=2) + "\n")


CPU = gate(f'100 * (1 - avg by(host) (rate({selector("node_cpu_seconds_total", "mode=\"idle\"")}[$__rate_interval])))', "cpu")
MEM = gate(f'100 * (1 - {selector("node_memory_MemAvailable_bytes")} / {selector("node_memory_MemTotal_bytes")})', "meminfo")
SWAP = gate(f'100 * (1 - {selector("node_memory_SwapFree_bytes")} / {selector("node_memory_SwapTotal_bytes")}) and ({selector("node_memory_SwapTotal_bytes")} > 0)', "meminfo")
# `type` is Service.Type (simple/notify/oneshot), not the unit category.
SERVICE_FAILED = 'name=~".+\\\\.service",state="failed"'
FAILED = gate(f'sum by(host) ({selector("node_systemd_unit_state", SERVICE_FAILED)})', "systemd")


def resources(d):
    d.row("Resources · gaps are unknown, not zero")
    d.panel("CPU busy", CPU, "percent", "Idle counter rate across logical CPUs; up and cpu collector gated.")
    d.panel("Memory used / total", MEM, "percent", "MemAvailable (includes reclaimable cache), not MemFree.", x=12, advance=True)
    d.panel("Swap used / total", SWAP, "percent", "No swap configured => Unknown/not applicable, not 0%.")
    load = gate(f'{selector("node_load1")} / on(host) group_left count by(host) ({selector("node_cpu_seconds_total", "mode=\"idle\"")})', "loadavg")
    load = f'({load}) and on(host) ({selector("node_scrape_collector_success", "collector=\"cpu\"")} == 1)'
    d.panel("Load 1m / logical CPU", load, description="Load is a gauge; 1 means one runnable/blocked task per logical CPU.", x=12, advance=True)


def services(d):
    d.row("System services · system manager only")
    d.panel("Failed services (all service units)", FAILED, description="Zero means observed healthy. Unknown means node/systemd collection failed. Does not include user-manager/Podman user units.", kind="stat", thresholds=[(None, "green"), (1, "red")])
    failures = gate(selector("node_systemd_unit_state", SERVICE_FAILED) + " == 1", "systemd")
    d.panel("Failed service identities", failures, description="Empty is normal when no failures; check the count/collector before interpreting emptiness.", kind="stat", legend="{{host}} · {{name}}", x=12, advance=True)
    # Filter fixed inventory by selected host without manufacturing health on collector failure.
    status = f'({service_status()}) and on(host) ({selector("up")} == 1)'
    d.panel("Required continuously-running services", status, description="0 absent exact unit; 1 present but not active; 2 active. Requires successful node/systemd collection. Curated baseline only; inactive oneshots/timers are not blanket failures.", kind="stat", legend="{{host}} · {{name}}", mappings=SERVICE_STATUS, w=24, height=9, advance=True)


def filesystems(d):
    d.row("Local filesystem capacity · no remote or duplicate snapshot mounts")
    d.panel("Usable space used", fs_capacity(), "percent", "1 - avail/size; excludes device errors and zero-size filesystems. /boot included; btrfs snapshots, bind aliases and NFS excluded.", legend="{{host}} · {{mountpoint}}", thresholds=[(None, "green"), (80, "orange"), (90, "red")])
    d.panel("Inodes used", fs_capacity(True), "percent", "Filesystems with zero/unsupported inode totals stay unknown (e.g. btrfs/vfat).", legend="{{host}} · {{mountpoint}}", x=12, advance=True, thresholds=[(None, "green"), (80, "orange"), (90, "red")])
    status = " or ".join(f"({fs_status(h)})" for h in HOSTS)
    d.panel("Expected mount / device health", f'({status}) and on(host) ({selector("up")} == 1)', kind="stat", legend="{{host}} · {{mountpoint}}", mappings=FS_STATUS, description="Explicit expected inventory. A disappeared mount is not a zero-used disk. Scrape/filesystem collector failure => Unknown.", w=12)
    d.panel("Read-only filesystems", gate(f'{fs_selector("node_filesystem_readonly")} and ({fs_selector("node_filesystem_device_error")} == 0)', "filesystem"), kind="stat", legend="{{host}} · {{mountpoint}}", description="1 is read-only; 0 is writable. Consult device/mount health for missing devices.", mappings=[(0, "Writable", "green"), (1, "Read-only", "red")], x=12, advance=True)


def gpu(d):
    d.row("Chungus NVIDIA GPU · not applicable to mini/lapdog")
    gup = selector("up", host="chungus", job="nvidia_gpu")
    code = selector("nvidia_smi_command_exit_code", host="chungus", job="nvidia_gpu")
    e = expected(("chungus",))
    d.panel("GPU exporter scrape", f'(max by(host) ({gup}) or on(host) {e}) + on(host) (count by(host) ({gup}) or on(host) {e})', kind="stat", mappings=STATUS)
    d.panel("nvidia-smi exit code", f'{code} and on(host) ({gup} == 1) and on(host) ({selector("up", host="chungus")} == 1)', kind="stat", mappings=[(0, "Success", "green")], description="Nonzero = command failure despite HTTP 200; absent = Unknown. GPU panels remain fleet-scoped even on host detail.", x=12, advance=True)

    def metric(name):
        return selector("nvidia_smi_" + name, host="chungus", job="nvidia_gpu")

    def ggate(expr):
        return gate(f'({expr}) and on(host) ({gup} == 1) and on(host) ({code} == 0)', host="chungus")

    d.panel("GPU utilization", ggate("100 * " + metric("utilization_gpu_ratio")), "percent", legend="{{host}} · {{uuid}}")
    d.panel("VRAM used / total", ggate(f'100 * {metric("memory_used_bytes")} / {metric("memory_total_bytes")}'), "percent", legend="{{host}} · {{uuid}}", x=12, advance=True)
    d.panel("GPU temperature", ggate(metric("temperature_gpu")), "celsius", legend="{{host}} · {{uuid}}")
    d.panel("GPU power draw", ggate(metric("power_draw_watts")), "watt", legend="{{host}} · {{uuid}}", x=12, advance=True)


def build():
    fleet = Dashboard("host-health-fleet", "Host health · Fleet overview")
    fleet.text("Read first", "Three fixed hosts: **mini · chungus · lapdog**. Reachability is a node scrape result, not proof of physical power state. Lapdog is **missing until its real LAN DHCP reservation/DNS/target are configured**. Resources and services are gated by scrape and collector success; gaps/Unknown are never healthy zeros. Historical graphs retain observed history. Select a host and use Host detail for I/O. Runbook: repository `docs/host-health.md`. GPU section is always Chungus-only.")
    fleet.row("Fixed fleet inventory · independent of discovered labels")
    for i, h in enumerate(HOSTS):
        fleet.panel(h + " · node reachability", presence((h,), h), kind="stat", mappings=STATUS, x=i * 8, w=8, height=4, advance=i == 2, description="Missing = no recent target series; Scrape failed = configured but unreachable; Reachable = last scrape succeeded. Fixed inventory ignores host filter.")
    resources(fleet)
    services(fleet)
    filesystems(fleet)
    gpu(fleet)
    fleet.save("fleet.json")

    detail = Dashboard("host-health-detail", "Host health · Host detail", detail=True)
    detail.text("Semantics", "Choose **mini/chungus/lapdog**, even if never scraped. Unknown resources are not zero. Current stats require a successful node and relevant collector scrape. Systemd is the node collector (not separate systemd_exporter). Disk I/O is whole-device; network is physical interfaces plus Tailscale. GPU section explicitly stays Chungus-only. Dashboard shows infrastructure, not end-to-end application availability.")
    detail.row("Current host / collector health")
    # Single custom variable has fixed options, including hosts with no scrape series.
    detail.panel("Node reachability", presence(("$host",)), kind="stat", mappings=STATUS, w=8, height=5)
    detail.panel("Uptime", gate(f'time() - {selector("node_boot_time_seconds")}', "stat"), "s", kind="stat", x=8, w=8, height=5)
    detail.panel("Collector success", gate(selector("node_scrape_collector_success", 'collector=~"cpu|meminfo|loadavg|filesystem|diskstats|netdev|systemd|stat"')), kind="stat", legend="{{collector}}", mappings=[(0, "Failed", "red"), (1, "Success", "green")], x=16, w=8, height=5, advance=True)
    resources(detail)
    detail.row("Network and block-device throughput")
    network = []
    for direction in ("receive", "transmit"):
        expr = selector(f"node_network_{direction}_bytes_total", 'device=~"en.*|eth.*|wl.*|tailscale0"')
        network.append((gate(f'rate({expr}[$__rate_interval])', "netdev"), "{{device}} · " + direction))
    detail.panel("Network throughput", network, "Bps", description="Per interface; bridges, veth and loopback excluded. Tailscale/physical traffic can overlap; do not sum.")
    disks = []
    for direction in ("read", "written"):
        expr = selector(f"node_disk_{direction}_bytes_total", 'device=~"nvme[0-9]+n[0-9]+|sd[a-z]+"')
        disks.append((gate(f'rate({expr}[$__rate_interval])', "diskstats"), "{{device}} · " + direction))
    detail.panel("Disk throughput", disks, "Bps", description="Whole devices only, not partitions. Counter rates; do not sum device and partition metrics.", x=12, advance=True)
    detail.panel("Disk busy time", gate(f'100 * rate({selector("node_disk_io_time_seconds_total", "device=~\"nvme[0-9]+n[0-9]+|sd[a-z]+\"")}[$__rate_interval])', "diskstats"), "percent", legend="{{device}}")
    detail.panel("Network errors / dropped packets", [(gate(f'rate({selector("node_network_" + direction + "_" + metric + "_total", "device=~\"en.*|eth.*|wl.*|tailscale0\"")}[$__rate_interval])', "netdev"), "{{device}} · " + direction + " " + metric) for direction in ("receive", "transmit") for metric in ("errs", "drop")], "pps", x=12, advance=True)
    services(detail)
    filesystems(detail)
    gpu(detail)
    detail.save("host.json")


if __name__ == "__main__":
    build()
