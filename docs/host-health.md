# Host health — first release

## Contract and layout

- Mini runs Grafana **13.3.0**, loopback `127.0.0.1:3001`, behind
  `https://grafana.local.doreto.com.br` (Caddy internal CA + Authentik).
  Trust the existing `home-caddy.crt` on clients. No Grafana firewall opening.
- Provisioned Prometheus datasource UID `victoriametrics` connects **server-side**
  to `http://127.0.0.1:8428`, with a 30s scrape interval. No plugin required.
- Immutable dashboards: `/d/host-health-fleet` and `/d/host-health-detail`, in
  **Host health**. Homepage links to fleet; series data links drill into a host.
  Source: `hosts/mini/services/grafana/`. Edit `generate.py`, regenerate JSON,
  run tests, and stage JSON before Nix evaluation. Do not edit dashboards in UI.
- Fixed inventory is **mini, chungus, lapdog**, not label discovery. Fleet
  reachability ignores filters deliberately. Host detail permits selecting a
  never-scraped host. GPU panels are explicitly fleet-scoped **Chungus only**,
  even on host detail; mini/lapdog are not assumed to have NVIDIA cards.
- Reachability: **Missing** = no recent node `up` series; **Scrape failed** =
  `up=0`; **Reachable** = `up=1`. This does not diagnose physical power state.
  VM/Grafana backend errors remain errors, not a healthy fallback.
- Resource queries never fill absent values with zero. Current stats and each
  historical point require node `up=1` plus the relevant collector success.
  GPU additionally requires GPU exporter `up=1` and nvidia-smi exit code 0.
  CPU/disk/network are counter rates; memory/load/filesystem/GPU are gauges.
  Historical graphs retain real past observations; nulls are not connected.
  A removed target can remain visible until VM's lookback/staleness expires;
  this is scrape freshness, not an independent heartbeat.

## Before deployment: manual Authentik setup (required)

Nix does **not** create Authentik applications/providers or access policies.

1. Create a **Proxy Provider → Forward auth (single application)** with external
   host `https://grafana.local.doreto.com.br`.
2. Associate an application; bind policies/groups restricting access to the
   intended users. Test both an allowed and a denied user.
3. Assign the application/provider to **authentik Embedded Outpost**. An
   unassigned application gives an outpost 404; startup races can give 502.
4. Apply the NixOS system generation manually using the usual deployment process.
   Check `systemctl status grafana-secret-setup grafana caddy authentik` and logs.

Every Grafana path (UI, `/api/*`, Live websocket handshake) is forward-auth
protected. The outpost callback path is routed **only** to Authentik. Caddy
removes caller-supplied `X-WEBAUTH-USER` and `X-Authentik-Username` before auth,
then copies only the successful outpost identity into `X-WEBAUTH-USER`.
Grafana trusts that header only from 127.0.0.1; there is no role header mapping.
Other mini processes are inside this trust boundary, not isolated tenants.

Proxy signup creates **Viewer** accounts; open signup, org creation, anonymous,
local basic auth and login form are disabled. Initial admin creation is
explicitly disabled: no new `admin/admin` account. First release has no normal
interactive Grafana administrator: dashboards/datasource are managed in Git.
**An existing Grafana database is not reset**: before reusing one, audit/remove
old local accounts, org memberships, server-admin flags and API/service-account
credentials. Existing elevated accounts are not demoted by auto-assign-role.
Any deliberate maintenance-admin recovery is a separate controlled procedure,
not a reason to enable public signup/basic auth or promote all SSO users.
Grafana logout alone does not end the Authentik browser session.

### Signing key and recovery

`grafana-secret-setup.service` runs as root before Grafana and creates
`/etc/secrets/grafana/secret-key` **only if absent**, using `openssl rand -hex 32`.
Directory mode 0700, root owner; file mode 0600. Atomic rename avoids partial
keys after interruption; empty existing files cause startup failure, not silent
rotation. Restarting/redeploying preserves the key. systemd `LoadCredential`
provides `/run/credentials/grafana.service/secret-key`; Grafana's `$__file{…}`
provider reads it. Secret contents never enter the Nix store, environment
literals, source control, or logs.

Back up the key securely alongside `/var/lib/grafana` (SQLite/state); protect
backup permissions and restore the matching pair. Do not delete the key as a
restart fix: changing it can invalidate sessions and encrypted datasource
secrets. Intentional rotation is a maintenance operation with reconfiguration
of encrypted secrets and session invalidation, not automatic on every boot.
Inspect permissions/credential **paths**, not contents, in support logs.

## Lapdog addressing blocker — intentionally missing today

No real stable lapdog LAN IP or profile MAC is recorded. Its exporter already
listens on 9100 and its firewall only permits mini's LAN address `192.168.0.2`.
Do not open exporter ports globally. Mini's two dnsmasq instances are **DNS
only**, despite having leasefile paths. Do not add a second DHCP server.

The operator must:

1. On lapdog and the actual router/DHCP server, inspect the connected LAN
   profile, real lease, interface and effective/cloned/randomized MAC policy
   (`nmcli connection show`, `nmcli device show`, router leases). Reserve a
   collision-free LAN IP for that profile at the **actual DHCP server**. Do not
   guess an IP/MAC or change the laptop's roaming profiles globally.
2. Add `/lapdog.home/<verified-reserved-IP>` to **LAN only** `address` in
   `hosts/mini/networking.nix`. Do not add it to Tailscale DNS. `no-hosts=true`
   means `/etc/hosts` is not sufficient. Unknown `.home` names currently forward
   to public upstreams; a router dynamic hostname is not sufficient either.
3. From mini verify `host lapdog.home 127.0.0.1` and
   `curl --fail http://lapdog.home:9100/metrics`; ensure it really is lapdog
   (`node_uname_info`), not mini. `lapdog.local.doreto.com.br` resolves through
   the service wildcard to **mini** and is **not** a valid laptop target.
4. Uncomment the prepared `lapdog.home:9100` node static target with
   `host: lapdog` in `hosts/mini/services/victoriametrics.nix`, validate, then
   manually deploy mini. There is no separate lapdog systemd_exporter target;
   the **node systemd collector** is the common three-host source.

Until those facts are available, no fake/unresolvable active target is installed
and the fixed dashboards report lapdog **Missing**, not healthy or omitted.
Sleeping/roaming lapdog will show failed scrapes once configured; that can be
expected, not necessarily an incident. Tailscale scraping or profile-scoped
static IPv4 would require a separate confirmed address/routing/firewall design.

## What is covered

- CPU busy, load per logical CPU, available-memory-based usage, swap (no swap
  means unknown/not applicable), uptime, collector success.
- Whole-device disk throughput/busy time and per-interface network throughput,
  drops/errors. Physical NICs plus Tailscale; do not sum overlapping traffic.
- Local filesystem usable space (`avail`, not `free`), inode capacity, read-only
  and explicit expected mount/device presence. `/` and `/boot` on all; Chungus
  also `/data` and `/mnt/snapraid-parity1`. Only ext4/btrfs/xfs/vfat; exclude NFS,
  snapshots and duplicate bind mounts. Zero-size/unsupported inode totals are
  unknown; an absent/error device is not zero-used space.
- All failed **service** units, plus a deliberately small continuously-running
  baseline: mini caddy/victoriametrics/grafana/node exporter/sshd; Chungus
  NetworkManager/sshd/node exporter/NVIDIA exporter; lapdog
  NetworkManager/sshd/node exporter. Status distinguishes missing exact unit,
  present but not active, and active, gated by node/systemd collection. Never
  classify all inactive oneshots/timers as failures. This is not a complete
  application inventory; update the curated list intentionally.
- Chungus GPU utilization, VRAM percentage, temperature and power from
  `nvidia_smi_*` (`uuid` identity), exporter reachability and command exit status.
  Unsupported/missing actual driver fields remain unknown. Verify live 4090
  schema; the upstream fixture is evidence for names, not hardware coverage.

## Validation before and after manual deployment

Local repeatable checks (use **host flake pins**, not `/etc/nix/inputs`):

```sh
python3 hosts/mini/services/grafana/generate.py
# Obtain prometheus.cli from hosts/mini's pinned nixpkgs, then:
python3 hosts/mini/services/grafana/test.py /path/to/promtool
make format-nix
# Stage all new modules/assets so Git flake source includes them.
nix flake check ./hosts/mini/
nix flake check ./hosts/lapdog/
```

`test.py` checks deterministic generation, identity/datasource/query contracts,
parses every dashboard expression using promtool, and exercises synthetic
counter/gauge, missing/failed scrape, collector failure, service missing/stopped,
filesystem/error/inode, and GPU success/command-failure/missing cases. It writes
scratch fixtures only under `.private/tmp`. This does not substitute for VM or
Grafana end-to-end testing. Validate generated scrape YAML with pinned VM
`-dryRun -promscrape.config=<file>` and generated Caddyfile with pinned Caddy
`adapt`/`validate`. For non-root validation, redirect only log/storage paths into
scratch space and disable local CA trust installation; never run a production
Caddy listener locally.

After deployment, check all of these before calling the release operational:

- `ss -lntp`: Grafana and VM loopback only. From another host, direct 3001 must
  fail and exporters must reject sources other than mini (mini uses loopback).
- VM `/targets` and `up{job="node"}` have the correct host identities; lapdog
  remains missing until the addressing prerequisite is completed. Inspect each
  live `/metrics`, especially node systemd/collector labels and GPU units.
- Query each dashboard expression through VM `/api/v1/query`/`query_range`;
  verify datasource connectivity, Grafana `/api/health` locally, provisioned
  dashboard UIDs, no provisioning errors, and client LAN/Tailscale DNS/CA trust.
- Without a browser session, request `/`, `/api/search`, and `/api/live/ws` via
  Caddy, both normally and spoofing the two identity headers (including mixed
  case). None may return authenticated Grafana data. A redirect/401/403 is
  expected; a successful Authentik login page is not a Grafana session.
- Confirm denied Authentik user cannot enter; allowed user has Viewer role via
  `/api/user`/`/api/user/orgs` and cannot save dashboards/edit datasource. Check
  no default administrator exists, no local login/basic bypass, outpost
  callbacks do not loop, and a Live websocket works when authenticated.
- During an approved Authentik outage test, Caddy must fail closed (e.g. 502),
  not pass unauthenticated traffic. Restore service promptly.
- In an approved window, stop an exporter or suspend a host: reachability
  becomes failed and current resource stats disappear rather than stay green.
  Omit a target and wait past staleness: fixed host becomes Missing. Simulate
  systemd/filesystem collection failure: dependent stats become unknown.
- Test failed/stopped/missing critical units with fixtures or a disposable
  noncritical test unit/list entry, **not by stopping SSH/Caddy on a live host**.
  Failed service count must agree with the identity list; inactive/missing
  expected services must not appear active. Test missing mount/device error
  with fixtures, not by unmounting production disks. Restore test config.
- Restart Grafana and repeat a manual deployment: secret checksum (privately
  compared, never published) stays unchanged and provisioned dashboards persist.

## Limits / separate security decision

No alerts/notifications, blackbox HTTP/TCP/DNS probes, TLS expiry, SMART/NVMe,
application-specific metrics, user-systemd/Podman-user unit enumeration, RAID
integrity or backup-success checks yet. An active system service does not prove
its web/API/database path works. These are follow-up probes/alerts, not implied
by a green infrastructure panel. The first release has no alert-rule pipeline.

**Existing exposure is unchanged:** `metrics.local.doreto.com.br` still exposes
the unauthenticated VM API (including write/admin capabilities); logs has a
similar existing reverse proxy. Protecting Grafana does **not** protect those
alternate endpoints or make the entire monitoring stack private. Choose their
access/write policy separately; this release intentionally does not change it.
