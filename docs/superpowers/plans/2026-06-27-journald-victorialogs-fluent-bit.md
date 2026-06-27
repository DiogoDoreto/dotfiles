# Journald To VictoriaLogs With Fluent Bit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace broken `systemd-journal-upload` log shipping with Fluent Bit journald shipping to VictoriaLogs across `mini`, `lapdog`, and `chungus`.

**Architecture:** Add one shared NixOS module that configures Fluent Bit's `systemd` input and `http` output for VictoriaLogs. Remove `services.journald.upload` first, then enable the new module in host order: `mini`, `lapdog`, `chungus`. Keep VictoriaLogs bound to localhost on `mini`; remote hosts ship through the existing Caddy endpoint.

**Tech Stack:** NixOS modules, Fluent Bit `systemd` input, Fluent Bit `http` output, VictoriaLogs `/insert/jsonline`, Caddy local TLS, Superpowers workbook documentation.

## Global Constraints

- Remove all `services.journald.upload` configuration before enabling Fluent Bit.
- Enable and verify host shipping in this order: `mini`, `lapdog`, `chungus`.
- Do not expose VictoriaLogs directly on the LAN; remote hosts must keep using `https://logs.local.doreto.com.br`.
- Use `services.fluent-bit`; do not hand-roll a custom systemd service for Fluent Bit.
- Use Fluent Bit filesystem state under `/var/lib/fluent-bit`.
- Use `/insert/jsonline`, not `/insert/journald`, for Fluent Bit ingestion.
- Update `.agents/skills/victoriatools-workbook/SKILL.md` before final validation.
- Run `make format-nix` after `.nix` edits.
- Run `nix flake check ./hosts/mini/`, `nix flake check ./hosts/lapdog/`, and `nix flake check ./hosts/chungus/` before completion.
- Build only `mini` and `lapdog`; do not build `chungus`.
- Do not run `nixos-rebuild switch`, `nixos-rebuild boot`, `home-manager switch`, or any activation command.

---

## File Structure

- Create `modules/nixos/services/journald-to-victorialogs.nix`: shared NixOS module that owns Fluent Bit journald shipping configuration.
- Modify `modules/nixos/default.nix`: import the new shared module.
- Modify `hosts/chungus/flake.nix`: import `../../modules/nixos` so `chungus` can use the shared `dog.services.*` option namespace.
- Modify `hosts/mini/services/victorialogs.nix`: remove `services.journald.upload` and enable local Fluent Bit shipping to `127.0.0.1:9428`.
- Modify `hosts/lapdog/configuration.nix`: remove `services.journald.upload` and enable remote Fluent Bit shipping through Caddy.
- Modify `hosts/chungus/services/monitoring.nix`: remove `services.journald.upload` and enable remote Fluent Bit shipping through Caddy.
- Modify `.agents/skills/victoriatools-workbook/SKILL.md`: replace `systemd-journal-upload` workbook guidance with Fluent Bit guidance.

### Task 1: Remove Broken Journald Upload Configuration

**Files:**
- Modify: `hosts/mini/services/victorialogs.nix:17-22`
- Modify: `hosts/lapdog/configuration.nix:203-209`
- Modify: `hosts/chungus/services/monitoring.nix:40-46`

**Interfaces:**
- Consumes: existing `services.journald.upload` host blocks.
- Produces: host configs with no `services.journald.upload` declarations, so the failing `systemd-journal-upload.service` is removed before the replacement is added.

- [ ] **Step 1: Run the failing removal check**

Run:

```bash
rg -n 'services\.journald\.upload|systemd-journal-upload|/insert/journald' hosts/mini hosts/lapdog hosts/chungus
```

Expected: FAIL for the intended end state by printing matches in `hosts/mini/services/victorialogs.nix`, `hosts/lapdog/configuration.nix`, and `hosts/chungus/services/monitoring.nix`.

- [ ] **Step 2: Remove the mini journald upload block**

Edit `hosts/mini/services/victorialogs.nix` so it becomes:

```nix
{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.victorialogs = {
    enable = true;
    listenAddress = "127.0.0.1:${p.victorialogs}";
    extraOptions = [
      "-retentionPeriod=3" # months
    ];
  };
}
```

- [ ] **Step 3: Remove the lapdog journald upload block**

In `hosts/lapdog/configuration.nix`, delete this full block:

```nix
  # Ship lapdog's journal to mini's VictoriaLogs via Caddy
  services.journald.upload = {
    enable = true;
    settings.Upload = {
      URL = "https://logs.local.doreto.com.br/insert/journald";
    };
  };
```

- [ ] **Step 4: Remove the chungus journald upload block**

In `hosts/chungus/services/monitoring.nix`, delete this full block:

```nix
  # Ship chungus journal logs to mini's VictoriaLogs via Caddy
  services.journald.upload = {
    enable = true;
    settings.Upload = {
      URL = "https://logs.local.doreto.com.br/insert/journald";
    };
  };
```

- [ ] **Step 5: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0.

- [ ] **Step 6: Verify upload config is gone from host Nix files**

Run:

```bash
rg -n 'services\.journald\.upload|/insert/journald' hosts/mini hosts/lapdog hosts/chungus --glob '*.nix'
```

Expected: no output and exit code 1 from `rg`.

- [ ] **Step 7: Check host flakes after removal**

Run:

```bash
nix flake check ./hosts/mini/
nix flake check ./hosts/lapdog/
nix flake check ./hosts/chungus/
```

Expected: each command exits 0. Warnings about a dirty Git tree are acceptable.

- [ ] **Step 8: Commit removal**

Run:

```bash
git status --short
git add hosts/mini/services/victorialogs.nix hosts/lapdog/configuration.nix hosts/chungus/services/monitoring.nix
git commit -m "monitoring: remove broken journald upload"
```

Expected: commit succeeds and includes only the three host config files.

### Task 2: Add Shared Fluent Bit Journald Module

**Files:**
- Create: `modules/nixos/services/journald-to-victorialogs.nix`
- Modify: `modules/nixos/default.nix:1-5`
- Modify: `hosts/chungus/flake.nix:60-69`

**Interfaces:**
- Consumes: NixOS `services.fluent-bit` module from nixpkgs.
- Produces: `dog.services.journald-to-victorialogs.enable`, `host`, `port`, and `tls` options.

- [ ] **Step 1: Run the failing module option checks**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.enable
```

Expected: both commands fail because the option does not exist yet. The `chungus` command also confirms `../../modules/nixos` is not currently imported there.

- [ ] **Step 2: Create the shared module**

Create `modules/nixos/services/journald-to-victorialogs.nix` with this exact content:

```nix
{
  config,
  lib,
  ...
}:

let
  cfg = config.dog.services.journald-to-victorialogs;
  ingestUri = "/insert/jsonline?_stream_fields=_HOSTNAME,_TRANSPORT&_msg_field=MESSAGE&_time_field=date";
in

{
  options.dog.services.journald-to-victorialogs = {
    enable = lib.mkEnableOption "journald shipping to VictoriaLogs with Fluent Bit";

    host = lib.mkOption {
      type = lib.types.str;
      default = "logs.local.doreto.com.br";
      description = "VictoriaLogs HTTP host for Fluent Bit to send logs to.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 443;
      description = "VictoriaLogs HTTP port for Fluent Bit to send logs to.";
    };

    tls = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether Fluent Bit should use TLS for the VictoriaLogs HTTP output.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.fluent-bit = {
      enable = true;
      settings = {
        service = {
          flush = 1;
          log_level = "info";
          "storage.path" = "/var/lib/fluent-bit/storage";
          "storage.type" = "filesystem";
          "storage.inherit" = "on";
          "storage.sync" = "normal";
          "storage.max_chunks_up" = 128;
        };

        pipeline = {
          inputs = [
            {
              name = "systemd";
              tag = "journal.*";
              db = "/var/lib/fluent-bit/journald.db";
              read_from_tail = true;
            }
          ];

          outputs = [
            ({
              name = "http";
              match = "journal.*";
              host = cfg.host;
              port = cfg.port;
              uri = ingestUri;
              format = "json_lines";
              json_date_key = "date";
              json_date_format = "iso8601";
              retry_limit = false;
              "storage.total_limit_size" = "100M";
            } // lib.optionalAttrs cfg.tls {
              tls = "on";
              "tls.verify" = "on";
            })
          ];
        };
      };
    };

    systemd.services.fluent-bit = {
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig.StateDirectory = "fluent-bit";
    };
  };
}
```

- [ ] **Step 3: Import the shared module from the NixOS module aggregator**

Edit `modules/nixos/default.nix` so it becomes:

```nix
{
  imports = [
    ./services/journald-to-victorialogs.nix
    ./services/opencode-agent-vm.nix
  ];
}
```

- [ ] **Step 4: Import shared NixOS modules into chungus**

Edit `hosts/chungus/flake.nix` so the `nixos-modules` list contains `../../modules/nixos` immediately after `(import ../../nix-config.nix inputs)`:

```nix
      nixos-modules = [
        (import ../../nix-config.nix inputs)
        ../../modules/nixos
        home-manager.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.sharedModules = home-manager-modules;
          nixpkgs = { inherit overlays; };
        }
        ./configuration.nix
      ];
```

- [ ] **Step 5: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0.

- [ ] **Step 6: Verify the option exists and defaults disabled**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.enable
```

Expected: each command prints `false`.

- [ ] **Step 7: Commit shared module**

Run:

```bash
git status --short
git add modules/nixos/services/journald-to-victorialogs.nix modules/nixos/default.nix hosts/chungus/flake.nix
git commit -m "monitoring: add Fluent Bit journal shipper module"
```

Expected: commit succeeds and includes only the shared module import changes.

### Task 3: Enable Fluent Bit Shipping On Mini

**Files:**
- Modify: `hosts/mini/services/victorialogs.nix`

**Interfaces:**
- Consumes: `dog.services.journald-to-victorialogs` options from Task 2.
- Produces: `mini` Fluent Bit config sending to local VictoriaLogs on `127.0.0.1:9428` without TLS.

- [ ] **Step 1: Run the failing mini enablement check**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.enable
```

Expected: prints `false`.

- [ ] **Step 2: Enable local shipping on mini**

Edit `hosts/mini/services/victorialogs.nix` so it becomes:

```nix
{ ... }:

let
  vars = import ../_variables.nix;
  p = builtins.mapAttrs (_: toString) vars.ports;
in

{
  services.victorialogs = {
    enable = true;
    listenAddress = "127.0.0.1:${p.victorialogs}";
    extraOptions = [
      "-retentionPeriod=3" # months
    ];
  };

  dog.services.journald-to-victorialogs = {
    enable = true;
    host = "127.0.0.1";
    port = vars.ports.victorialogs;
    tls = false;
  };
}
```

- [ ] **Step 3: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0.

- [ ] **Step 4: Verify mini Fluent Bit config evaluates**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.services.fluent-bit.enable
nix eval --raw ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.host
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.port
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.dog.services.journald-to-victorialogs.tls
```

Expected output:

```text
true
true
127.0.0.1
9428
false
```

- [ ] **Step 5: Check and build mini**

Run:

```bash
nix flake check ./hosts/mini/
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
```

Expected: both commands exit 0. Warnings about a dirty Git tree are acceptable.

- [ ] **Step 6: Commit mini enablement**

Run:

```bash
git status --short
git add hosts/mini/services/victorialogs.nix
git commit -m "monitoring: ship mini journal with Fluent Bit"
```

Expected: commit succeeds and includes only `hosts/mini/services/victorialogs.nix`.

### Task 4: Enable Fluent Bit Shipping On Lapdog

**Files:**
- Modify: `hosts/lapdog/configuration.nix`

**Interfaces:**
- Consumes: `dog.services.journald-to-victorialogs` options from Task 2.
- Produces: `lapdog` Fluent Bit config sending to `logs.local.doreto.com.br:443` with TLS.

- [ ] **Step 1: Run the failing lapdog enablement check**

Run:

```bash
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.enable
```

Expected: prints `false`.

- [ ] **Step 2: Enable remote shipping on lapdog**

In `hosts/lapdog/configuration.nix`, add this block immediately after the `services.prometheus.exporters.node` block and before `services.tailscale.enable = true;`:

```nix
  dog.services.journald-to-victorialogs = {
    enable = true;
    host = "logs.local.doreto.com.br";
    port = 443;
    tls = true;
  };
```

- [ ] **Step 3: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0.

- [ ] **Step 4: Verify lapdog Fluent Bit config evaluates**

Run:

```bash
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.services.fluent-bit.enable
nix eval --raw ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.host
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.port
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.dog.services.journald-to-victorialogs.tls
```

Expected output:

```text
true
true
logs.local.doreto.com.br
443
true
```

- [ ] **Step 5: Check and build lapdog**

Run:

```bash
nix flake check ./hosts/lapdog/
nix build ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
```

Expected: both commands exit 0. Warnings about a dirty Git tree are acceptable.

- [ ] **Step 6: Commit lapdog enablement**

Run:

```bash
git status --short
git add hosts/lapdog/configuration.nix
git commit -m "monitoring: ship lapdog journal with Fluent Bit"
```

Expected: commit succeeds and includes only `hosts/lapdog/configuration.nix`.

### Task 5: Enable Fluent Bit Shipping On Chungus

**Files:**
- Modify: `hosts/chungus/services/monitoring.nix`

**Interfaces:**
- Consumes: `dog.services.journald-to-victorialogs` options from Task 2.
- Produces: `chungus` Fluent Bit config sending to `logs.local.doreto.com.br:443` with TLS.

- [ ] **Step 1: Run the failing chungus enablement check**

Run:

```bash
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.enable
```

Expected: prints `false`.

- [ ] **Step 2: Enable remote shipping on chungus**

In `hosts/chungus/services/monitoring.nix`, add this block after the exporter definitions and before the firewall comment:

```nix
  dog.services.journald-to-victorialogs = {
    enable = true;
    host = "logs.local.doreto.com.br";
    port = 443;
    tls = true;
  };
```

The resulting middle of the file should look like this:

```nix
  services.prometheus.exporters.nvidia-gpu = {
    enable = true;
    listenAddress = "0.0.0.0";
    port = nvidiaExporterPort;
  };

  dog.services.journald-to-victorialogs = {
    enable = true;
    host = "logs.local.doreto.com.br";
    port = 443;
    tls = true;
  };

  # Allow only mini (192.168.0.2) to scrape the exporters on this host.
```

- [ ] **Step 3: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0.

- [ ] **Step 4: Verify chungus Fluent Bit config evaluates**

Run:

```bash
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.enable
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.services.fluent-bit.enable
nix eval --raw ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.host
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.port
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.dog.services.journald-to-victorialogs.tls
```

Expected output:

```text
true
true
logs.local.doreto.com.br
443
true
```

- [ ] **Step 5: Check chungus without building it**

Run:

```bash
nix flake check ./hosts/chungus/
```

Expected: command exits 0. Do not run `nix build` for `chungus`.

- [ ] **Step 6: Commit chungus enablement**

Run:

```bash
git status --short
git add hosts/chungus/services/monitoring.nix
git commit -m "monitoring: ship chungus journal with Fluent Bit"
```

Expected: commit succeeds and includes only `hosts/chungus/services/monitoring.nix`.

### Task 6: Update VictoriaTools Workbook

**Files:**
- Modify: `.agents/skills/victoriatools-workbook/SKILL.md`

**Interfaces:**
- Consumes: final Fluent Bit topology from Tasks 2 through 5.
- Produces: operational docs for debugging `fluent-bit.service`, `/var/lib/fluent-bit/journald.db`, `/var/lib/fluent-bit/storage`, and `/insert/jsonline` ingestion.

- [ ] **Step 1: Run the failing workbook freshness check**

Run:

```bash
rg -n 'systemd-journal-upload|journald upload|/insert/journald|fluent-bit.service' .agents/skills/victoriatools-workbook/SKILL.md
```

Expected: output mentions `systemd-journal-upload`, `journald upload`, and `/insert/journald`; output does not mention `fluent-bit.service`.

- [ ] **Step 2: Update the architecture overview**

Replace `.agents/skills/victoriatools-workbook/SKILL.md` lines 15-38 with:

```markdown
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
```

- [ ] **Step 3: Update the data locations table**

Replace the data locations table with:

```markdown
| Path | Purpose |
|---|---|
| `/var/lib/victoriametrics/` | TSDB data (NixOS service data directory) |
| `/var/lib/victorialogs/` | VictoriaLogs storage (systemd StateDirectory) |
| `/var/lib/fluent-bit/journald.db` | Fluent Bit journald cursor database on each shipping host |
| `/var/lib/fluent-bit/storage/` | Fluent Bit filesystem buffer on each shipping host |
```

- [ ] **Step 4: Update the first-response checklist**

In the first-response checklist, add Fluent Bit status and logs after the VictoriaLogs/Caddy status lines:

```bash
# Log shipper health on each shipping host
systemctl status fluent-bit.service
journalctl -u fluent-bit.service -n 50 --no-pager
```

Replace the old key interpretation for VictoriaLogs port 9428 with:

```markdown
- **Remote Fluent Bit cannot send logs** - VictoriaLogs binds to 127.0.0.1 on
  mini. Remote hosts must ship through Caddy at
  `https://logs.local.doreto.com.br/insert/jsonline`. Check
  `fluent-bit.service` logs on the shipping host and Caddy logs on mini.
- **`systemd-journal-upload.service` exists or is failing** - this stack no
  longer uses it. Remove old `services.journald.upload` config and use the
  shared Fluent Bit module instead.
```

- [ ] **Step 5: Replace the Journald Upload section**

Replace the section beginning with `## Journald Upload (Log Shipping)` through the end of `### Common Journal Upload Issues` with:

```markdown
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

- **`fluent-bit.service` cannot read journal entries** - confirm the service has
  the `systemd-journal` supplementary group. The upstream NixOS
  `services.fluent-bit` module sets this.
- **No cursor DB or storage directory exists** - verify the shared module adds
  `StateDirectory = "fluent-bit"` and that `/var/lib/fluent-bit/` exists after
  the service starts.
- **Remote hosts cannot connect to VictoriaLogs** - check DNS resolution for
  `logs.local.doreto.com.br`, Caddy health on mini, and whether the host trusts
  `hosts/mini/home-caddy.crt` via `security.pki.certificateFiles`.
- **VictoriaLogs receives no rows** - inspect `journalctl -u fluent-bit.service`
  for HTTP status errors, then query VictoriaLogs metrics for
  `/insert/jsonline` request activity.
- **Filesystem buffer grows during an outage** - this is expected while Caddy or
  VictoriaLogs is unavailable. The output queue is bounded by
  `storage.total_limit_size` in the shared Fluent Bit module.
- **`systemd-journal-upload.service` is present** - this is legacy config. This
  repository no longer uses `services.journald.upload` for VictoriaLogs.
```

- [ ] **Step 6: Verify workbook no longer points operators at the old endpoint**

Run:

```bash
rg -n '/insert/journald|services\.journald\.upload|systemctl cat systemd-journal-upload' .agents/skills/victoriatools-workbook/SKILL.md
rg -n 'fluent-bit.service|/insert/jsonline|/var/lib/fluent-bit' .agents/skills/victoriatools-workbook/SKILL.md
```

Expected: first command has no output and exits 1. Second command prints multiple Fluent Bit and `/insert/jsonline` references.

- [ ] **Step 7: Commit workbook update**

Run:

```bash
git status --short
git add .agents/skills/victoriatools-workbook/SKILL.md
git commit -m "victoriatools: document Fluent Bit journal shipping"
```

Expected: commit succeeds and includes only `.agents/skills/victoriatools-workbook/SKILL.md`.

### Task 7: Final Validation

**Files:**
- Verify: all files changed by Tasks 1 through 6.

**Interfaces:**
- Consumes: committed implementation from Tasks 1 through 6.
- Produces: validated repo state ready for user deployment.

- [ ] **Step 1: Check final git status**

Run:

```bash
git status --short
```

Expected: no uncommitted implementation changes. If `docs/superpowers/plans/2026-06-27-journald-victorialogs-fluent-bit.md` is untracked, leave it uncommitted unless the user asks to commit the plan.

- [ ] **Step 2: Run final formatting**

Run:

```bash
make format-nix
```

Expected: command exits 0 and does not modify files.

- [ ] **Step 3: Verify legacy Nix config is gone**

Run:

```bash
rg -n 'services\.journald\.upload|/insert/journald' hosts modules --glob '*.nix'
```

Expected: no output and exit code 1 from `rg`.

- [ ] **Step 4: Verify Fluent Bit module exists in all target host configs**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.services.fluent-bit.enable
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.services.fluent-bit.enable
nix eval ./hosts/chungus#nixosConfigurations.chungus.config.services.fluent-bit.enable
```

Expected output:

```text
true
true
true
```

- [ ] **Step 5: Run all host flake checks**

Run:

```bash
nix flake check ./hosts/mini/
nix flake check ./hosts/lapdog/
nix flake check ./hosts/chungus/
```

Expected: each command exits 0. Warnings about a dirty Git tree are acceptable only if the plan file remains untracked.

- [ ] **Step 6: Build allowed systems**

Run:

```bash
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
nix build ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
```

Expected: both commands exit 0. Do not build `chungus`.

- [ ] **Step 7: Summarize deployment commands for the user**

Report that implementation validation passed and that the user must manually deploy hosts in this order:

```text
1. mini
2. lapdog
3. chungus
```

Also report that the user, not the agent, must run `nixos-rebuild switch` or any other activation command.
