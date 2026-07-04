# Mini Split Dnsmasq Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run two dnsmasq instances on mini so LAN clients get LAN service IPs and Tailscale clients get Tailscale service IPs.

**Architecture:** Replace mini's single built-in `services.dnsmasq` process with two explicit systemd services. Each service gets a generated dnsmasq config, binds only to its network-specific listen address set, validates the config in `preStart`, and runs dnsmasq in foreground mode without DBus.

**Tech Stack:** NixOS host flake, Nix module configuration, systemd services, dnsmasq, `pkgs.formats.keyValue`, `nix flake check`.

## Global Constraints

- Host-specific NixOS changes go in `hosts/mini/networking.nix`.
- Do not modify `hosts/mini/hardware.nix`.
- Do not run `nixos-rebuild switch`, `nixos-rebuild boot`, `home-manager switch`, or any activation command.
- Format Nix files with `make format-nix` after editing `.nix` files.
- Validate mini with `nix flake check ./hosts/mini/`.
- Mini local builds are allowed for verification.
- Keep LAN answer for `*.local.doreto.com.br` as `192.168.0.2`.
- Keep Tailscale answer for `*.local.doreto.com.br` as `100.117.142.110`.
- Keep query logging enabled on both dnsmasq instances.
- Do not enable dnsmasq DBus for either custom instance.

---

## File Structure

- Modify `hosts/mini/networking.nix`: owns mini network services, firewall DNS port exposure, Tailscale enablement, OpenVPN DNS route protection, and the dnsmasq split-instance systemd services.
- No new Nix module is needed because this is host-specific and does not currently benefit another host.
- No shared helper is needed because the split dnsmasq pattern is used once.

---

### Task 1: Replace Single Dnsmasq With Split Instances

**Files:**
- Modify: `hosts/mini/networking.nix:1-144`

**Interfaces:**
- Consumes: existing mini values `vars.chungusProxyIp`, `config.networking.hostName`, `pkgs.dnsmasq`, `pkgs.coreutils`, and `lib.generators.mkKeyValueDefault`.
- Produces: systemd services `dnsmasq-lan.service` and `dnsmasq-tailscale.service`; system user `dnsmasq`; local resolver nameserver `127.0.0.1`.

- [ ] **Step 1: Run the failing eval for the LAN service**

Run:

```bash
nix eval --raw ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.dnsmasq-lan.description
```

Expected: command fails because `config.systemd.services.dnsmasq-lan` does not exist yet. A dirty Git tree warning is acceptable.

- [ ] **Step 2: Run the failing eval for the Tailscale service**

Run:

```bash
nix eval --raw ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.dnsmasq-tailscale.description
```

Expected: command fails because `config.systemd.services.dnsmasq-tailscale` does not exist yet. A dirty Git tree warning is acceptable.

- [ ] **Step 3: Add `lib`, shared dnsmasq config generation, and static IP bindings**

In `hosts/mini/networking.nix`, change the module header from:

```nix
{ config, pkgs, ... }:
```

to:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
```

Then replace the existing `let` block from `let` through the end of `keepDnsOffVpn` with this block:

```nix
let
  vars = import ./_variables.nix;
  lanGateway = "192.168.0.1";
  lanInterface = "wlo1";
  staticIp = "192.168.0.2"; # needs to be set manually in networkmanager
  tailscaleIp = "100.117.142.110";
  dnsUpstreams = [
    "1.1.1.1"
    "1.0.0.1"
  ];
  dnsmasqStateDir = "/var/lib/dnsmasq";
  dnsmasqLanLeaseFile = "${dnsmasqStateDir}/dnsmasq-lan.leases";
  dnsmasqTailscaleLeaseFile = "${dnsmasqStateDir}/dnsmasq-tailscale.leases";

  dnsmasqSettingsFormat =
    let
      formatKeyValue =
        name: value:
        if value == true then
          name
        else if value == false then
          "# setting `${name}` explicitly set to false"
        else
          lib.generators.mkKeyValueDefault { } "=" name value;
    in
    pkgs.formats.keyValue {
      mkKeyValue = formatKeyValue;
      listsAsDuplicateKeys = true;
    };

  commonDnsmasqSettings = {
    bind-dynamic = true;
    no-hosts = true;
    no-resolv = true;
    bogus-priv = true;
    domain-needed = true;
    server = dnsUpstreams;
    log-queries = true;
  };

  mkDnsmasqConfig =
    name: leaseFile: settings:
    dnsmasqSettingsFormat.generate "${name}.conf" (
      commonDnsmasqSettings
      // {
        dhcp-leasefile = leaseFile;
        pid-file = "/run/${name}.pid";
      }
      // settings
    );

  dnsmasqLanConfig = mkDnsmasqConfig "dnsmasq-lan" dnsmasqLanLeaseFile {
    listen-address = [
      "::1"
      "127.0.0.1"
      staticIp
      vars.chungusProxyIp
    ];
    address = [
      "/${config.networking.hostName}.home/${staticIp}"
      "/local.doreto.com.br/${staticIp}"
      "/chungus.home/192.168.0.3"
      "/chungus-proxy.home/${vars.chungusProxyIp}"
    ];
  };

  dnsmasqTailscaleConfig = mkDnsmasqConfig "dnsmasq-tailscale" dnsmasqTailscaleLeaseFile {
    listen-address = [ tailscaleIp ];
    address = [ "/local.doreto.com.br/${tailscaleIp}" ];
  };

  mkDnsmasqService =
    {
      description,
      configFile,
      leaseFile,
      after ? [
        "network.target"
        "systemd-resolved.service"
      ],
      wants ? [ ],
    }:
    {
      inherit description after wants;
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.dnsmasq ];
      preStart = ''
        mkdir -m 755 -p ${dnsmasqStateDir}
        touch ${leaseFile}
        chown -R dnsmasq ${dnsmasqStateDir}
        dnsmasq --test -C ${configFile}
      '';
      serviceConfig = {
        ExecStart = "${pkgs.dnsmasq}/bin/dnsmasq -k --user=dnsmasq -C ${configFile}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        PrivateTmp = true;
        ProtectSystem = true;
        ProtectHome = true;
        Restart = "always";
      };
    };

  keepDnsOffVpn = pkgs.writeShellScript "keep-dns-off-vpn" ''
    set -eu

    action="$1"
    for upstream in ${builtins.concatStringsSep " " dnsUpstreams}; do
      case "$action" in
        up)
          ${pkgs.iproute2}/bin/ip route replace "$upstream/32" via ${lanGateway} dev ${lanInterface}
          ;;
        down)
          ${pkgs.iproute2}/bin/ip route del "$upstream/32" via ${lanGateway} dev ${lanInterface} 2>/dev/null || true
          ;;
      esac
    done
  '';
in
```

- [ ] **Step 4: Preserve local resolver behavior**

Inside the existing top-level `networking` attribute set in `hosts/mini/networking.nix`, add `nameservers` immediately after `hostName`:

```nix
    hostName = "dogdot";
    nameservers = [ "127.0.0.1" ];
```

- [ ] **Step 5: Replace the built-in dnsmasq module block**

In `hosts/mini/networking.nix`, replace the entire existing `services.dnsmasq` attribute set from line 74 through line 122 with this code:

```nix
  services.dnsmasq.enable = false;

  users.users.dnsmasq = {
    isSystemUser = true;
    group = "dnsmasq";
    description = "Dnsmasq daemon user";
  };
  users.groups.dnsmasq = { };

  systemd.services.dnsmasq-lan = mkDnsmasqService {
    description = "Dnsmasq LAN DNS";
    configFile = dnsmasqLanConfig;
    leaseFile = dnsmasqLanLeaseFile;
  };

  systemd.services.dnsmasq-tailscale = mkDnsmasqService {
    description = "Dnsmasq Tailscale DNS";
    configFile = dnsmasqTailscaleConfig;
    leaseFile = dnsmasqTailscaleLeaseFile;
    after = [
      "network-online.target"
      "tailscaled.service"
      "systemd-resolved.service"
    ];
    wants = [
      "network-online.target"
      "tailscaled.service"
    ];
  };
```

Leave the existing `systemd.services.chungus-proxy-iface` block starting at line 124 unchanged.

- [ ] **Step 6: Format Nix files**

Run:

```bash
make format-nix
```

Expected: command exits 0. The formatter may rewrite spacing in `hosts/mini/networking.nix`.

- [ ] **Step 7: Verify the LAN service evaluates**

Run:

```bash
nix eval --raw ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.dnsmasq-lan.description
```

Expected: command exits 0 and prints:

```text
Dnsmasq LAN DNS
```

- [ ] **Step 8: Verify the Tailscale service evaluates**

Run:

```bash
nix eval --raw ./hosts/mini#nixosConfigurations.dogdot.config.systemd.services.dnsmasq-tailscale.description
```

Expected: command exits 0 and prints:

```text
Dnsmasq Tailscale DNS
```

- [ ] **Step 9: Verify the built-in dnsmasq service is disabled**

Run:

```bash
nix eval ./hosts/mini#nixosConfigurations.dogdot.config.services.dnsmasq.enable
```

Expected: command exits 0 and prints:

```text
false
```

- [ ] **Step 10: Run the mini flake check**

Run:

```bash
nix flake check ./hosts/mini/
```

Expected: command exits 0.

- [ ] **Step 11: Build the mini system closure**

Run:

```bash
nix build ./hosts/mini#nixosConfigurations.dogdot.config.system.build.toplevel
```

Expected: command exits 0 and creates or updates the local `result` symlink.

- [ ] **Step 12: Inspect the diff before committing**

Run:

```bash
git status --short
git diff -- hosts/mini/networking.nix
```

Expected: `hosts/mini/networking.nix` is modified. Pre-existing unrelated files may also appear in `git status`; do not stage them.

- [ ] **Step 13: Commit only the Nix implementation**

Run:

```bash
git add -- hosts/mini/networking.nix
git commit -m "mini: split dnsmasq by network"
```

Expected: commit succeeds with only `hosts/mini/networking.nix` staged.

---

### Task 2: Post-Deployment DNS Verification

**Files:**
- Modify: none

**Interfaces:**
- Consumes: deployed mini system generation containing `dnsmasq-lan.service` and `dnsmasq-tailscale.service`.
- Produces: evidence that each network receives only its reachable DNS answer.

- [ ] **Step 1: Check both dnsmasq services on mini**

Run on mini after the user activates the new generation:

```bash
systemctl status dnsmasq-lan.service dnsmasq-tailscale.service --no-pager
```

Expected: both units show `active (running)`.

- [ ] **Step 2: Query the LAN listener**

Run from a LAN client or on mini:

```bash
dig @192.168.0.2 local.doreto.com.br +short
```

Expected output:

```text
192.168.0.2
```

- [ ] **Step 3: Query the Tailscale listener**

Run from a Tailscale client:

```bash
dig @100.117.142.110 local.doreto.com.br +short
```

Expected output:

```text
100.117.142.110
```

- [ ] **Step 4: Confirm the LAN listener does not return the Tailscale IP**

Run from a LAN client or on mini:

```bash
dig @192.168.0.2 local.doreto.com.br +short | grep -Fx 100.117.142.110
```

Expected: command exits non-zero because the LAN listener does not return `100.117.142.110`.

- [ ] **Step 5: Confirm the Tailscale listener does not return the LAN IP**

Run from a Tailscale client:

```bash
dig @100.117.142.110 local.doreto.com.br +short | grep -Fx 192.168.0.2
```

Expected: command exits non-zero because the Tailscale listener does not return `192.168.0.2`.

- [ ] **Step 6: Check query logs if any DNS answer is wrong**

Run on mini:

```bash
journalctl -u dnsmasq-lan.service -u dnsmasq-tailscale.service -g 'query' --no-pager
```

Expected: query log entries show which service received each DNS query.
