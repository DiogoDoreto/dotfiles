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

Mini runs VictoriaMetrics and VictoriaLogs (both native NixOS services) as
the central monitoring hub. Each host ships its own metrics and logs to mini:

| Host | IP | Exports | Ships |
|---|---|---|---|
| mini | 192.168.0.2 | node_exporter (127.0.0.1), systemd_exporter (127.0.0.1) | journal -> Fluent Bit -> VictoriaLogs on 127.0.0.1 |
| lapdog | DHCP | node_exporter (0.0.0.0) | journal -> Fluent Bit -> VictoriaLogs via Caddy (logs.local.doreto.com.br) |
| chungus | 192.168.0.3 | node_exporter (0.0.0.0), systemd_exporter (0.0.0.0), nvidia_smi (0.0.0.0) | journal -> Fluent Bit -> VictoriaLogs via Caddy (logs.local.doreto.com.br) |

All remote exporters are scraped by VictoriaMetrics over the LAN. Remote
journald logs ship through Caddy to mini's VictoriaLogs. VictoriaLogs remains
bound to 127.0.0.1 on mini.

Config files:

| File | Purpose |
|---|---|
| `modules/nixos/services/journald-to-victorialogs.nix` | Shared Fluent Bit journald shipper module |
| `hosts/mini/services/victoriametrics.nix` | VictoriaMetrics service, scrape config, retention |
| `hosts/mini/services/victorialogs.nix` | VictoriaLogs service and mini's Fluent Bit shipper target |
| `hosts/mini/services/monitoring-exporters.nix` | node_exporter + systemd_exporter on mini |
| `hosts/lapdog/configuration.nix` | node_exporter + Fluent Bit journal shipping on lapdog |
| `hosts/chungus/services/monitoring.nix` | Exporters, Fluent Bit journal shipping, firewall on chungus |
| `hosts/mini/caddy.nix` | Reverse proxy for `metrics.local.doreto.com.br` and `logs.local.doreto.com.br` |
| `hosts/mini/_variables.nix` | Port registry for all monitoring services |

## Ports

| Port | Service | Listens On |
|---|---|---|
| `8428` | VictoriaMetrics HTTP | 127.0.0.1 (mini) |
| `9428` | VictoriaLogs HTTP | 127.0.0.1 (mini) |
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
| `/var/lib/victorialogs/` | VictoriaLogs storage (systemd StateDirectory) |
| `/var/lib/fluent-bit/journald.db` | Fluent Bit journald cursor database on each shipping host |
| `/var/lib/fluent-bit/storage/` | Fluent Bit filesystem buffer on each shipping host |

## First-Response Checklist

```bash
# Service health
systemctl status victoriametrics.service
systemctl status victorialogs.service caddy.service

# Is VictoriaMetrics listening?
ss -tlnp | grep 8428

# Is VictoriaLogs listening?
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
journalctl -u victorialogs.service -n 50 --no-pager

# Log shipper health on each shipping host
systemctl status fluent-bit.service
journalctl -u fluent-bit.service -n 50 --no-pager
```

Key things to look for:

- **`victorialogs.service` not found** — the service module is not enabled.
  Check that `services.victorialogs.enable = true` is set in the Nix config
  and that the host was switched.
- **502 from Caddy for metrics or logs endpoints** — the backend service is not
  listening or crashed. Check `ss -tlnp` and the service statuses above.
- **`curl: (6) Could not resolve host`** — DNS issue. Ensure dnsmasq on mini is
  serving `*.local.doreto.com.br`.
- **Remote Fluent Bit cannot send logs** — VictoriaLogs binds to 127.0.0.1 on
  mini. Remote hosts must ship through Caddy at
  `https://logs.local.doreto.com.br/insert/jsonline`. Check
  `fluent-bit.service` logs on the shipping host and Caddy logs on mini.
- **`systemd-journal-upload.service` exists or is failing** — this stack no
  longer uses it. Remove old `services.journald.upload` config and use the
  shared Fluent Bit module instead.
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

## VictoriaLogs (NixOS Service)

VictoriaLogs runs as a native NixOS service (`services.victorialogs`) managed
by systemd as `victorialogs.service`. The package version is pinned by the
nixpkgs revision in the flake input.

VictoriaLogs query output is newline-delimited JSON. Do not pipe raw
`/select/logsql/query` output to `python3 -m json.tool` unless the endpoint
returns a single JSON object or array.

### Service Commands

```bash
# View service logs
journalctl -u victorialogs.service -n 50 --no-pager

# Follow live logs
journalctl -u victorialogs.service -f

# Restart the service
sudo systemctl restart victorialogs.service

# Check storage usage
sudo du -sh /var/lib/victorialogs/
```

## Journald Log Shipping

All hosts ship journald logs with Fluent Bit, not `systemd-journal-upload`.
Fluent Bit reads the local systemd journal with the `systemd` input and sends
JSON-line records to VictoriaLogs' `/insert/jsonline` endpoint.

Mini ships directly to `http://127.0.0.1:9428/insert/jsonline`. Lapdog and
chungus ship through Caddy at `https://logs.local.doreto.com.br/insert/jsonline`.
Caddy provides TLS termination with its internal CA, which lapdog and chungus
trust via `security.pki.certificateFiles`.

Rollout order for the current stack is mini, then lapdog, then chungus.

### Checking Fluent Bit Status

```bash
# On the shipping host
systemctl status fluent-bit.service
journalctl -u fluent-bit.service -n 100 --no-pager

# Check local Fluent Bit state
sudo ls -lh /var/lib/fluent-bit/
sudo du -sh /var/lib/fluent-bit/storage/
```

### Checking VictoriaLogs Ingestion

```bash
# Recent logs for a host
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _HOSTNAME:mini | limit 20'

curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _HOSTNAME:lapdog | limit 20'

curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _HOSTNAME:chungus | limit 20'

# Unit-specific lookup
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _SYSTEMD_UNIT:fluent-bit.service | limit 20'
```

### Common Fluent Bit Shipping Issues

- **`fluent-bit.service` cannot read journal entries** — confirm the service has
  the `systemd-journal` supplementary group. The upstream NixOS
  `services.fluent-bit` module sets this.
- **No cursor DB or storage directory exists** — verify the shared module adds
  `StateDirectory = "fluent-bit"` and that `/var/lib/fluent-bit/` exists after
  the service starts.
- **Remote hosts cannot connect to VictoriaLogs** — check DNS resolution for
  `logs.local.doreto.com.br`, Caddy health on mini, and whether the host trusts
  `hosts/mini/home-caddy.crt` via `security.pki.certificateFiles`.
- **VictoriaLogs receives no rows** — inspect `journalctl -u fluent-bit.service`
  for HTTP status errors, then query VictoriaLogs metrics for
  `/insert/jsonline` request activity.
- **Filesystem buffer grows during an outage** — this is expected while Caddy or
  VictoriaLogs is unavailable. The output queue is bounded by
  `storage.total_limit_size` in the shared Fluent Bit module.
- **`systemd-journal-upload.service` is present** — this is legacy config. This
  repository no longer uses `services.journald.upload` for VictoriaLogs.

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
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m | limit 10'

# Filter by host (via journald _HOSTNAME field)
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:1h _HOSTNAME:chungus | limit 20'

# Filter by unit
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:1h _SYSTEMD_UNIT:caddy.service | limit 20'
```

### Home Assistant Lookups

Home Assistant on mini runs as a Home Manager user Podman container:

| Item | Value |
|---|---|
| Container unit | `podman-home-assistant.service` |
| Journald identifier | `home-assistant` |
| Config mount | `/home/dog/projects/home-assistant-config/config:/config` |
| Caddy upstream | `192.168.0.2:8123` |
| Local URL | `https://ha.local.doreto.com.br` |

Fast failure query:

```bash
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:8h (SYSLOG_IDENTIFIER:home-assistant OR _SYSTEMD_USER_UNIT:podman-home-assistant.service OR _SYSTEMD_UNIT:caddy.service) (ha.local.doreto.com.br OR "8123" OR "Failed to create HTTP server" OR "connection refused" OR "Name does not resolve") | limit 120'
```

Focused follow-up queries:

```bash
# Caddy side: confirms whether Caddy is reachable but HA is not listening.
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:4h _SYSTEMD_UNIT:caddy.service (ha.local.doreto.com.br OR "192.168.0.2:8123" OR "connection refused" OR "502") | limit 80'

# Home Assistant side: HTTP bind and startup errors.
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:4h SYSLOG_IDENTIFIER:home-assistant (error OR "8123" OR "HTTP server" OR "Name does not resolve") | limit 80'

# Container lifecycle: restart/update context.
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:4h (_SYSTEMD_USER_UNIT:podman-home-assistant.service OR SYSLOG_IDENTIFIER:podman) home-assistant | limit 100'

# Home Manager activation: shows whether the user container stack was restarted.
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:4h _SYSTEMD_UNIT:home-manager-dog.service | limit 80'
```

Key interpretations:

- **Caddy `dial tcp 192.168.0.2:8123: connect: connection refused`** means
  Caddy is reachable, but Home Assistant is not listening on port 8123.
- **Home Assistant `Failed to create HTTP server at port 8123: [Errno -2] Name does not resolve`**
  usually means `http.server_host` contains a hostname that cannot resolve.
- **Container `create/start/died/remove` events** around the same timestamp show
  whether the failure followed a reboot, Home Manager activation, or container update.
- **Home Manager user Podman containers** often show up under `user@1000.service`;
  search by `SYSLOG_IDENTIFIER` and `_SYSTEMD_USER_UNIT`, not only `_SYSTEMD_UNIT`.

## Firewall Rules Summary

| Host | Rule | Purpose |
|---|---|---|
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
| `/insert/jsonline` | JSON-line ingestion endpoint (used by Fluent Bit) |
| `/select/logsql/query` | LogsQL query (POST with `query` param) |
| `/select/logsql/hits` | Count of matching log entries |
| `/select/logsql/tail` | Live tail of matching entries (SSE) |
