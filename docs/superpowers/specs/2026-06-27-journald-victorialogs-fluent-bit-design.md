# Journald To VictoriaLogs With Fluent Bit

## Summary

Replace the failing `services.journald.upload` configuration with a per-host Fluent Bit journald shipper. Fluent Bit will read each host's local systemd journal and send JSON-line log records to VictoriaLogs on `mini`.

The rollout must remove the broken `services.journald.upload` configuration first, then enable Fluent Bit in host order: `mini`, `lapdog`, `chungus`.

## Context

`services.journald.upload` manages `systemd-journal-upload.service`. That service is designed for `systemd-journal-remote`, not arbitrary HTTP ingestion endpoints. It rewrites the configured target by adding systemd journal-remote URL semantics such as `/upload` and the default port `19532`, which makes it incompatible with VictoriaLogs' `/insert/journald` endpoint.

The current monitoring stack already has the right central shape:

- `mini` runs VictoriaLogs bound to `127.0.0.1:9428`.
- Caddy exposes VictoriaLogs as `https://logs.local.doreto.com.br`.
- `lapdog` and `chungus` trust the Caddy local CA.
- Host metrics are already centralized in VictoriaMetrics on `mini`.

The replacement should preserve this topology and avoid exposing VictoriaLogs directly on the LAN.

## Selected Approach

Use Fluent Bit as the journald log shipper.

Fluent Bit is the best fit because it has:

- A packaged NixOS module, `services.fluent-bit`.
- A native `systemd` input for reading journald.
- Cursor persistence via a local DB file.
- HTTP output support with JSON-lines format.
- VictoriaLogs-documented ingestion via `/insert/jsonline`.
- Bounded filesystem buffering and retry controls.

Vector remains the documented fallback if Fluent Bit's emitted field shape or runtime behavior proves unsuitable. Promtail is rejected because it is end-of-life and removed from current nixpkgs. `vmagent` is rejected because it is for metrics, not logs. `vlagent` is not the first choice because it does not act as the journald reader for this NixOS host setup.

## Architecture

Each host ships its own journal:

- `mini`: `journald -> fluent-bit -> http://127.0.0.1:9428/insert/jsonline`
- `lapdog`: `journald -> fluent-bit -> https://logs.local.doreto.com.br/insert/jsonline`
- `chungus`: `journald -> fluent-bit -> https://logs.local.doreto.com.br/insert/jsonline`

VictoriaLogs remains reachable only through its existing localhost listener on `mini` and through Caddy for remote hosts.

## Nix Design

Add a shared NixOS module at `modules/nixos/services/journald-to-victorialogs.nix`, and import it from `modules/nixos/default.nix`.

The module should expose this small host-level interface:

- `dog.services.journald-to-victorialogs.enable`
- `dog.services.journald-to-victorialogs.host`
- `dog.services.journald-to-victorialogs.port`
- `dog.services.journald-to-victorialogs.tls`

The module should configure `services.fluent-bit.settings` and add systemd service settings needed by this repository's deployment.

Required service state:

- `StateDirectory = "fluent-bit"`
- journald cursor DB at `/var/lib/fluent-bit/journald.db`
- filesystem buffer at `/var/lib/fluent-bit/storage`

`services.fluent-bit` already uses `DynamicUser = true` and adds `systemd-journal` as a supplementary group. The shared module must add persistent writable state because the Fluent Bit systemd unit otherwise has no stable path for cursor and buffer data.

## Fluent Bit Pipeline

Use the `systemd` input.

Recommended input settings:

- `name: systemd`
- `tag: journal.*`
- `db: /var/lib/fluent-bit/journald.db`
- `read_from_tail: true`

`read_from_tail: true` prevents an accidental first-run backfill of all retained journals. After the cursor DB exists, Fluent Bit resumes from the stored cursor.

Use the `http` output.

Recommended output settings:

- `name: http`
- `match: journal.*`
- `format: json_lines`
- `json_date_key: date`
- `json_date_format: iso8601`
- `uri: /insert/jsonline?_stream_fields=_HOSTNAME,_TRANSPORT&_msg_field=MESSAGE&_time_field=date`
- `retry_limit: false`
- `storage.total_limit_size: 100M`

For `mini`, use `host: 127.0.0.1`, `port: 9428`, and no TLS.

For `lapdog` and `chungus`, use `host: logs.local.doreto.com.br`, `port: 443`, and TLS enabled.

The design keeps `_SYSTEMD_UNIT`, `SYSLOG_IDENTIFIER`, `PRIORITY`, and other journald fields as normal queryable fields. `_SYSTEMD_UNIT` is not used as a stream field because kernel and other non-unit log messages may not have it, and templated units can increase stream cardinality.

## Error Handling

Use filesystem buffering for resilience across short outages and service restarts.

Recommended service settings:

- `flush: 1`
- `log_level: info`
- `storage.path: /var/lib/fluent-bit/storage`
- `storage.type: filesystem`
- `storage.inherit: on`
- `storage.sync: normal`
- `storage.max_chunks_up: 128`

The HTTP output must set `retry_limit: false`, otherwise Fluent Bit can discard chunks after its retry limit. The output queue should be bounded with `storage.total_limit_size: 100M` so a long outage cannot consume unlimited disk space.

The default exponential backoff settings are acceptable initially. They can be tuned later if Fluent Bit logs show noisy retries or slow recovery.

## Rollout Plan

The implementation must remove the known-broken upload service before adding the replacement shipper.

1. Remove all `services.journald.upload` configuration from `mini`, `lapdog`, and `chungus`.
2. Add the shared Fluent Bit module.
3. Enable and verify Fluent Bit on `mini`.
4. Enable and verify Fluent Bit on `lapdog`.
5. Enable and verify Fluent Bit on `chungus`.
6. Update `victoriatools-workbook` so it documents Fluent Bit rather than `systemd-journal-upload`.

## Verification

After each host is enabled, verify the local service:

```sh
systemctl status fluent-bit.service
journalctl -u fluent-bit.service -n 100 --no-pager
```

Verify that VictoriaLogs receives data:

```sh
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _HOSTNAME:<host> | limit 20'
```

Verify unit-specific fields remain queryable:

```sh
curl -s 'https://logs.local.doreto.com.br/select/logsql/query' \
  --data-urlencode 'query=_time:5m _SYSTEMD_UNIT:fluent-bit.service | limit 20'
```

On `mini`, check VictoriaLogs ingestion metrics for row growth and `/insert/jsonline` request activity.

## Documentation Updates

Update the `victoriatools-workbook` skill after the implementation.

The workbook must be changed to describe:

- Fluent Bit as the journald shipper.
- `fluent-bit.service` health checks.
- `/var/lib/fluent-bit/journald.db` and `/var/lib/fluent-bit/storage` state.
- VictoriaLogs ingestion through `/insert/jsonline`.
- The rollout order for host shipping: `mini`, `lapdog`, `chungus`.
- The removal of `systemd-journal-upload.service` from this stack.

## Open Risks

Fluent Bit's exact journald field output should be checked on the first host before enabling the others. If field names differ from the expected uppercase journald names, adjust `_msg_field`, `_stream_fields`, or input settings during the `mini` rollout.

If Fluent Bit TLS verification does not pick up the system trust store for Caddy's local CA on remote hosts, configure its TLS CA file explicitly or switch to the system bundle path used elsewhere in this repo.

If Fluent Bit's journald input behaves poorly with this host's journal volume, switch to the documented Vector fallback before rolling out to additional hosts.
