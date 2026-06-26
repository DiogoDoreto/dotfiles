---
name: victoriatools-workbook
description: >
  Operational workbook for debugging and maintaining the VictoriaMetrics / VictoriaLogs
  monitoring stack on the mini/dogdot server. Use when the user reports VictoriaMetrics,
  VictoriaLogs, metrics scraping, journald log shipping, node/systemd/nvidia exporters,
  or the metrics.local.doreto.com.br / logs.local.doreto.com.br endpoints are broken or
  behaving unexpectedly.
---

# VictoriaTools Workbook

## Architecture Overview

Mini runs VictoriaMetrics (native NixOS service) and VictoriaLogs (Podman
container) as the central monitoring hub. Each host ships its own metrics and
logs to mini:

| Host | IP | Exports | Ships |
|---|---|---|---|
| mini | 192.168.0.2 | node_exporter (127.0.0.1), systemd_exporter (127.0.0.1) | journal → VictoriaLogs on 127.0.0.1 |
| chungus | 192.168.0.3 | node_exporter (0.0.0.0), systemd_exporter (0.0.0.0), nvidia_smi (0.0.0.0) | journal → VictoriaLogs on 192.168.0.2 |
| lapdog | DHCP | node_exporter (0.0.0.0) | journal → VictoriaLogs on 192.168.0.2 |

All remote exporters are scraped by VictoriaMetrics over the LAN. Remote
journald upload targets mini's VictoriaLogs directly. Both services are
proxied through Caddy.

Config files:

| File | Purpose |
|---|---|
| `hosts/mini/services/victoriametrics.nix` | VictoriaMetrics service, scrape config, retention |
| `hosts/mini/services/victorialogs.nix` | VictoriaLogs Podman container, firewall rules, mini's own journald upload |
| `hosts/mini/services/monitoring-exporters.nix` | node_exporter + systemd_exporter on mini |
| `hosts/chungus/services/monitoring.nix` | Exporters, journald upload, firewall on chungus |
| `hosts/lapdog/configuration.nix` (lines 179–207) | node_exporter + journald upload on lapdog |
| `hosts/mini/caddy.nix` | Reverse proxy for `metrics.local.doreto.com.br` and `logs.local.doreto.com.br` |
| `hosts/mini/_variables.nix` | Port registry for all monitoring services |

## Ports

| Port | Service | Listens On |
|---|---|---|
| `8428` | VictoriaMetrics HTTP | 127.0.0.1 (mini) |
| `9428` | VictoriaLogs HTTP | 0.0.0.0 (mini, via Podman port mapping) |
| `9100` | node_exporter | 127.0.0.1 (mini), 0.0.0.0 (chungus, lapdog) |
| `9558` | systemd_exporter | 127.0.0.1 (mini), 0.0.0.0 (chungus) |
| `9835` | nvidia_smi exporter | 0.0.0.0 (chungus) |

## Caddy Endpoints

| URL | Proxies To | Purpose |
|---|---|---|
| `https://metrics.local.doreto.com.br` | `127.0.0.1:8428` | VictoriaMetrics UI |
| `https://logs.local.doreto.com.br` | `127.0.0.1:9428` | VictoriaLogs UI |

Both use Caddy's internal TLS (`tls internal`) via the `*.local.doreto.com.br`
wildcard in `caddy.nix`.

## Data Locations

| Path | Purpose |
|---|---|
| `/var/lib/victoriametrics/` | TSDB data (NixOS service data directory) |
| `/var/lib/victorialogs/` | VictoriaLogs storage, bind-mounted into container at `/vlogs` |

## First-Response Checklist

```bash
# Service health
systemctl status victoriametrics.service
systemctl status podman-victorialogs.service caddy.service

# Is VictoriaMetrics listening?
ss -tlnp | grep 8428

# Is VictoriaLogs container running and port bound?
sudo podman ps -a --filter name=victorialogs
ss -tlnp | grep 9428

# VictoriaMetrics self-metrics (local)
curl -s http://127.0.0.1:8428/metrics | head -20

# VictoriaMetrics health
curl -s http://127.0.0.1:8428/health

# VictoriaLogs health (direct)
curl -s http://127.0.0.1:9428/health

# Through Caddy (use local CA bundle for TLS)
curl -s --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
  https://metrics.local.doreto.com.br/health

curl -s --cacert /etc/ssl/certs/ca-bundle-with-local-ca.crt \
  https://logs.local.doreto.com.br/health

# Recent logs for both services
journalctl -u victoriametrics.service -n 50 --no-pager
journalctl -u podman-victorialogs.service -n 50 --no-pager
```

Key things to look for:

- **`podman-victorialogs.service` not found** — the container was never created.
  Check that `virtualisation.oci-containers.containers.victorialogs` is enabled
  in the Nix config and that the host was switched.
- **502 from Caddy for metrics or logs endpoints** — the backend service is not
  listening or crashed. Check `ss -tlnp` and the service statuses above.
- **`curl: (6) Could not resolve host`** — DNS issue. Ensure dnsmasq on mini is
  serving `*.local.doreto.com.br`.
- **VictoriaLogs port 9428 unreachable from chungus/lapdog** — firewall rule on
  mini may not be applied. Check `iptables -L nixos-fw` and verify the IP is in
  `192.168.0.0/24`.
- **Chungus exporters unreachable from mini** — chungus firewall may be blocking.
  Check `iptables -L nixos-fw` on chungus and verify the source IP `192.168.0.2`
  is allowed.

## Scrape Configuration

VictoriaMetrics is configured with an embedded `-promscrape.config` pointing at
a Nix-generated YAML file (`victoriametrics-scrape.yml`). Targets are defined
statically in `hosts/mini/services/victoriametrics.nix`:

- **node** job: mini (127.0.0.1), chungus (192.168.0.3), lapdog (commented out — DHCP)
- **systemd** job: mini (127.0.0.1), chungus (192.168.0.3)
- **nvidia_gpu** job: chungus only (192.168.0.3)

### Viewing Scrape Status

```bash
# List all targets and their scrape state
curl -s http://127.0.0.1:8428/targets | python3 -m json.tool

# Simplified: just target labels and health
curl -s http://127.0.0.1:8428/api/v1/targets \
  | python3 -c "
import json, sys
data = json.load(sys.stdin)
for ep in data['data']['activeTargets'] + data['data']['droppedTargets']:
    health = ep.get('health', 'unknown')
    labels = ep.get('labels', {})
    print(f\"{health:8s}  {labels.get('job','?'):15s}  {labels.get('host','?'):10s}  {labels.get('instance','?')}\")
"

# Check specific exporter from mini
curl -s http://127.0.0.1:9100/metrics | head -5
curl -s http://192.168.0.3:9100/metrics | head -5
curl -s http://192.168.0.3:9835/metrics | head -5
```

### Adding a New Exporter Target

1. **On the host:** enable the exporter service (e.g., `services.prometheus.exporters.node`)
   and ensure it listens on an appropriate interface.
2. **On that host's firewall:** allow mini (192.168.0.2) to reach the exporter port.
3. **On mini:** add a `static_configs` entry in `hosts/mini/services/victoriametrics.nix`
   under the appropriate job. Rebuild and switch mini.
4. **Verify**: `curl -s http://127.0.0.1:8428/targets` and confirm the new target
   shows `health: up`.

### Uncommenting Lapdog

Lapdog gets a dynamic LAN IP, so it is commented out in the scrape config.
To add it:

```bash
# On lapdog, find its LAN IP
ip addr show | grep 'inet 192.168'

# On mini, edit hosts/mini/services/victoriametrics.nix and uncomment the
# lapdog target block, replacing <lapdog-ip> with the actual address.

# Or assign a static DHCP lease in mini's dnsmasq config.
```

## VictoriaLogs (Podman Container)

VictoriaLogs runs as an OCI container managed by the NixOS `virtualisation.oci-containers`
module. It is **not** a native NixOS service — the systemd unit is
`podman-victorialogs.service` (auto-generated by the module).

### Upgrading VictoriaLogs

Since `image = "docker.io/victoriametrics/victoria-logs:latest"`, the container
uses whatever `latest` was pulled at creation time. To upgrade:

```bash
# Pull the new image
sudo podman pull docker.io/victoriametrics/victoria-logs:latest

# Restart the container to use the new image
sudo systemctl restart podman-victorialogs.service

# Verify
sudo podman ps --filter name=victorialogs
curl -s http://127.0.0.1:9428/health
```

No Nix rebuild is needed for image upgrades — only when changing container
parameters (ports, volumes, cmd) in the Nix config.

### Container Commands

```bash
# View container logs (same as journalctl -u podman-victorialogs.service)
sudo podman logs victorialogs

# Shell into the container
sudo podman exec -it victorialogs /bin/sh

# Check storage usage inside the container
sudo podman exec victorialogs du -sh /vlogs

# Check container resource usage
sudo podman stats --no-stream victorialogs
```

## Journald Upload (Log Shipping)

Both chungus and lapdog ship their systemd journals to mini's VictoriaLogs via
`services.journald.upload`. Mini also ships its own journal to localhost.

### Checking Journal Upload Status

```bash
# On the shipping host
systemctl status systemd-journal-upload.service
journalctl -u systemd-journal-upload.service -n 30 --no-pager

# Check if VictoriaLogs is receiving data
# VictoriaLogs UI: https://logs.local.doreto.com.br
# Or query the API directly
curl -s 'http://127.0.0.1:9428/select/logsql/query' \
  --data-urlencode 'query=_time:5m'
```

### Common Journal Upload Issues

- **`systemd-journal-upload.service` failing on chungus/lapdog** — mini's
  VictoriaLogs port (9428) may be unreachable. Check firewall on mini:
  ```bash
  sudo iptables -L nixos-fw | grep 9428
  ```
  The rule in `victorialogs.nix` allows `192.168.0.0/24`.

- **VictoriaLogs receiving no data** — the journald upload URL must point to
  the `/insert/journald` endpoint. Verify the URL in the shipping host's config:
  ```bash
  systemctl cat systemd-journal-upload.service | grep URL
  ```

- **VictoriaLogs disk filling up** — retention is set to 3 months. Check:
  ```bash
  sudo du -sh /var/lib/victorialogs/
  ```

## VictoriaMetrics Retention

Retention is set to 6 months in `victoriametrics.nix`:
```nix
retentionPeriod = "6"; # months
```

To check current data size:
```bash
sudo du -sh /var/lib/victoriametrics/
```

To change retention, edit `retentionPeriod` and rebuild/switch mini. Valid
units: `h` (hours), `d` (days), `w` (weeks), `m` (months), `y` (years).

## Querying Metrics (PromQL / MetricsQL)

```bash
# VictoriaMetrics supports both PromQL and MetricsQL through the same API

# Instant query
curl -s 'http://127.0.0.1:8428/api/v1/query?query=up'

# Range query (last hour, 1m step)
curl -s 'http://127.0.0.1:8428/api/v1/query_range?query=node_cpu_seconds_total&start=-1h&end=now&step=1m'

# List all metric names
curl -s 'http://127.0.0.1:8428/api/v1/label/__name__/values' | python3 -m json.tool

# List label values for a metric
curl -s 'http://127.0.0.1:8428/api/v1/label/host/values' | python3 -m json.tool
```

## Querying Logs (LogsQL)

```bash
# VictoriaLogs uses LogsQL for querying
# See: https://docs.victoriametrics.com/victorialogs/logsql/

# Recent log entries
curl -s 'http://127.0.0.1:9428/select/logsql/query' \
  --data-urlencode 'query=_time:5m | limit 10' | python3 -m json.tool

# Filter by host (via journald _HOSTNAME field)
curl -s 'http://127.0.0.1:9428/select/logsql/query' \
  --data-urlencode 'query=_time:1h _HOSTNAME:chungus | limit 20' | python3 -m json.tool

# Filter by unit
curl -s 'http://127.0.0.1:9428/select/logsql/query' \
  --data-urlencode 'query=_time:1h _SYSTEMD_UNIT:caddy.service | limit 20'
```

## Firewall Rules Summary

| Host | Rule | Purpose |
|---|---|---|
| mini | `192.168.0.0/24 tcp dport 9428 accept` | Allow LAN journald upload to VictoriaLogs |
| chungus | `192.168.0.2 tcp dport 9100,9558,9835 accept` | Allow mini to scrape chungus exporters |
| lapdog | `192.168.0.2 tcp dport 9100 accept` | Allow mini to scrape lapdog node_exporter |

## Useful VictoriaMetrics Endpoints

| Endpoint | Description |
|---|---|
| `/health` | Health check |
| `/metrics` | VictoriaMetrics own metrics in Prometheus format |
| `/targets` | HTML page with scrape target status |
| `/api/v1/targets` | JSON scrape target status |
| `/api/v1/query` | Instant query (PromQL/MetricsQL) |
| `/api/v1/query_range` | Range query |
| `/api/v1/label/__name__/values` | All metric names |
| `/api/v1/label/<name>/values` | Values for a given label |
| `/api/v1/series` | Time series matching a selector |
| `/api/v1/status/tsdb` | TSDB stats (series count, etc.) |
| `/-/reload` | Reload scrape config (POST) |

## Useful VictoriaLogs Endpoints

| Endpoint | Description |
|---|---|
| `/health` | Health check |
| `/insert/journald` | Journald ingestion endpoint |
| `/select/logsql/query` | LogsQL query (POST with `query` param) |
| `/select/logsql/hits` | Count of matching log entries |
| `/select/logsql/tail` | Live tail of matching entries (SSE) |