# OpenCode Agent MicroVM Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a reusable NixOS module that runs `opencode serve` inside an isolated microVM with explicit filesystem shares and VM-owned credentials.

**Architecture:** A shared NixOS module exposes `dog.services.opencode-agent-vm` and contains both host-side and guest-side configuration gated by separate enable flags. `lapdog` imports the shared module, enables the host side, and adds a new `opencode-agent-vm` guest NixOS configuration that imports the same module with the guest side enabled.

**Tech Stack:** NixOS modules, Home Manager, `microvm.nix`, QEMU TAP networking, systemd, dnsmasq, nftables, socat, OpenSSH, OpenCode from `llm-agents.nix`.

## Global Constraints

- Initial consumer: `lapdog` only.
- Keep the existing `lapdog-agent` microVM in place.
- Do not mount host `~/.ssh`, `~/.config/opencode`, `~/.local/share/opencode`, `~/.claude`, `~/.claude.json`, `.agents`, or personal secret directories into the new VM.
- Initial `lapdog` share: `/home/dog/projects` on the host to `/workspace/projects` in the guest, read-write.
- VM state directory: `/var/lib/opencode-agent-vm/`.
- Guest user: `agent` with UID `1000` and GID `1000`.
- Guest user has passwordless sudo inside the VM.
- Host-local web exposure on `lapdog`: bind only to `127.0.0.1`.
- Do not run `nixos-rebuild switch`, `nixos-rebuild boot`, `home-manager switch`, or any activation command.
- Do not build `chungus` or `inspiron7520`.
- Do not commit unless the user explicitly requests commits.

---

## File Structure

- Create: `modules/nixos/default.nix`
- Responsibility: shared NixOS module aggregator for this repo.

- Create: `modules/nixos/services/opencode-agent-vm.nix`
- Responsibility: option definitions plus host and guest implementation for the OpenCode agent microVM.

- Modify: `hosts/lapdog/flake.nix`
- Responsibility: import shared NixOS modules, define shared `opencodeAgentVm` settings, and add the `opencode-agent-vm` guest NixOS configuration.

- Modify: `hosts/lapdog/configuration.nix`
- Responsibility: enable the host side of the service, pass `self` as the microVM flake source, configure the initial `lapdog` share through shared settings, and make existing dnsmasq bridge values mergeable.

---

### Task 1: Add Shared NixOS Module Skeleton

**Files:**
- Create: `modules/nixos/default.nix`
- Create: `modules/nixos/services/opencode-agent-vm.nix`
- Modify: `hosts/lapdog/flake.nix`

**Interfaces:**
- Consumes: existing `hosts/lapdog/flake.nix` `nixos-modules` list.
- Produces: `dog.services.opencode-agent-vm` option namespace with `enable`, `guest.enable`, networking, share, credential, and exposure options.

- [x] **Step 1: Run failing option evaluation**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.options.dog.services.opencode-agent-vm.enable.description --raw
```

Expected: FAIL with an attribute-missing error for `dog` or `opencode-agent-vm`.

- [x] **Step 2: Create the shared NixOS module aggregator**

Create `modules/nixos/default.nix`:

```nix
{
  imports = [
    ./services/opencode-agent-vm.nix
  ];
}
```

- [x] **Step 3: Create the option skeleton**

Create `modules/nixos/services/opencode-agent-vm.nix`:

```nix
{
  config,
  lib,
  pkgs,
  self ? null,
  ...
}:

with lib;

let
  cfg = config.dog.services.opencode-agent-vm;

  shareType = types.submodule {
    options = {
      tag = mkOption {
        type = types.str;
        description = "Unique virtiofs tag for the share.";
      };

      source = mkOption {
        type = types.str;
        description = "Host path to share with the guest.";
      };

      mountPoint = mkOption {
        type = types.str;
        description = "Absolute guest path where the share is mounted.";
      };

      readOnly = mkOption {
        type = types.bool;
        default = false;
        description = "Mount the share read-only inside the guest.";
      };
    };
  };

  dnsForwardZoneType = types.submodule {
    options = {
      domain = mkOption {
        type = types.str;
        description = "DNS domain suffix forwarded to a specific upstream.";
      };

      server = mkOption {
        type = types.str;
        description = "DNS server used for the domain suffix.";
      };
    };
  };
in
{
  options.dog.services.opencode-agent-vm = {
    enable = mkEnableOption "OpenCode agent MicroVM host integration";

    guest.enable = mkEnableOption "OpenCode agent MicroVM guest runtime";

    vmName = mkOption {
      type = types.str;
      default = "opencode-agent-vm";
      description = "Name of the declarative microVM and systemd microvm@ unit.";
    };

    flake = mkOption {
      type = types.nullOr types.unspecified;
      default = self;
      defaultText = literalExpression "self";
      description = "Flake containing nixosConfigurations.<vmName> for the guest.";
    };

    stateDir = mkOption {
      type = types.str;
      default = "/var/lib/opencode-agent-vm";
      description = "Host directory for VM-owned persistent state and credentials.";
    };

    controlUser = mkOption {
      type = types.str;
      default = "dog";
      description = "Host user allowed to start, stop, and SSH into the VM through helper commands.";
    };

    guestUser = mkOption {
      type = types.str;
      default = "agent";
      description = "Normal user inside the guest VM.";
    };

    guestUid = mkOption {
      type = types.ints.positive;
      default = 1000;
      description = "UID for the guest user.";
    };

    guestGid = mkOption {
      type = types.ints.positive;
      default = 1000;
      description = "GID for the guest user primary group.";
    };

    guestHostname = mkOption {
      type = types.str;
      default = "opencode-agent-vm";
      description = "Hostname assigned inside the guest VM.";
    };

    bridgeName = mkOption {
      type = types.str;
      default = "opencode-vm0";
      description = "Host bridge used by the VM TAP interface.";
    };

    tapName = mkOption {
      type = types.str;
      default = "opencode-agent0";
      description = "Host TAP interface created by microvm.nix for this VM.";
    };

    hostAddress = mkOption {
      type = types.str;
      default = "10.0.101.1";
      description = "IPv4 address assigned to the host bridge.";
    };

    guestAddress = mkOption {
      type = types.str;
      default = "10.0.101.2";
      description = "Static IPv4 address assigned to the guest VM.";
    };

    networkCidr = mkOption {
      type = types.str;
      default = "10.0.101.0/24";
      description = "IPv4 CIDR for NAT and audit rules covering the VM network.";
    };

    prefixLength = mkOption {
      type = types.ints.between 1 32;
      default = 24;
      description = "IPv4 prefix length for the VM network.";
    };

    guestMac = mkOption {
      type = types.str;
      default = "02:00:00:00:01:02";
      description = "MAC address assigned to the guest TAP interface.";
    };

    vcpu = mkOption {
      type = types.ints.positive;
      default = 4;
      description = "Number of virtual CPUs assigned to the VM.";
    };

    mem = mkOption {
      type = types.ints.positive;
      default = 16384;
      description = "Memory assigned to the VM, in megabytes.";
    };

    opencodePort = mkOption {
      type = types.port;
      default = 32859;
      description = "Port where opencode serve listens inside the guest.";
    };

    hostLocalAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host address for the local-only proxy to the guest OpenCode server.";
    };

    hostLocalPort = mkOption {
      type = types.port;
      default = 32860;
      description = "Host port for local browser access to the guest OpenCode server.";
    };

    workingDirectory = mkOption {
      type = types.str;
      default = "/workspace/projects";
      description = "Working directory for the opencode serve systemd service inside the guest.";
    };

    shares = mkOption {
      type = types.listOf shareType;
      default = [ ];
      description = "Explicit host directories shared into the guest.";
    };

    dnsUpstreams = mkOption {
      type = types.listOf types.str;
      default = [
        "1.1.1.1"
        "1.0.0.1"
      ];
      description = "Default upstream DNS servers for VM DNS queries.";
    };

    dnsForwardZones = mkOption {
      type = types.listOf dnsForwardZoneType;
      default = [ ];
      description = "Domain-specific DNS forwarding rules for VM DNS queries.";
    };

    dnsmasqResolveLocalQueries = mkOption {
      type = types.bool;
      default = false;
      description = "Whether this module should let dnsmasq alter host local DNS resolution.";
    };

    audit.enable = mkOption {
      type = types.bool;
      default = true;
      description = "Log VM DNS queries and new outbound TCP/UDP connections on the host.";
    };

    caddy = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Expose the host-local OpenCode proxy through Caddy with Authentik forward_auth.";
      };

      hostName = mkOption {
        type = types.str;
        default = "opencode.local.doreto.com.br";
        description = "Caddy virtual host for remote OpenCode access.";
      };

      authentikUrl = mkOption {
        type = types.str;
        default = "http://localhost:9000";
        description = "Base URL for the Authentik outpost used by Caddy forward_auth.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.flake != null;
          message = "dog.services.opencode-agent-vm.flake must be set when the host side is enabled.";
        }
        {
          assertion = cfg.shares != [ ];
          message = "dog.services.opencode-agent-vm.shares must explicitly list every shared host directory.";
        }
      ];
    })

    (mkIf cfg.guest.enable {
      assertions = [
        {
          assertion = cfg.shares != [ ];
          message = "dog.services.opencode-agent-vm.shares must be passed to the guest configuration.";
        }
      ];
    })
  ];
}
```

- [x] **Step 4: Import shared NixOS modules in the lapdog flake**

Modify `hosts/lapdog/flake.nix` inside the `nixos-modules = [` list:

```diff
       nixos-modules = [
         (import ../../nix-config.nix inputs)
+        ../../modules/nixos
         home-manager.nixosModules.home-manager
         {
```

- [x] **Step 5: Verify the option exists**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.options.dog.services.opencode-agent-vm.enable.description --raw
```

Expected: PASS and prints `Whether to enable OpenCode agent MicroVM host integration.`

- [x] **Step 6: Commit gate**

Run only if the user explicitly requested commits:

```sh
git add modules/nixos/default.nix modules/nixos/services/opencode-agent-vm.nix hosts/lapdog/flake.nix
git commit -m "nix: add opencode agent vm module skeleton"
```

Expected: commit created. If commits were not requested, leave the changes uncommitted.

---

### Task 2: Implement Host-Side VM Integration

**Files:**
- Modify: `modules/nixos/services/opencode-agent-vm.nix`
- Modify: `hosts/lapdog/flake.nix`
- Modify: `hosts/lapdog/configuration.nix`

**Interfaces:**
- Consumes: `dog.services.opencode-agent-vm` options from Task 1.
- Produces: `systemd.services.opencode-agent-vm-init`, `systemd.services.microvm-bridge-opencode-agent-vm`, `systemd.services.opencode-agent-vm-proxy`, `microvm.vms.opencode-agent-vm`, DNS/audit host networking, and helper commands.

- [x] **Step 1: Add shared lapdog settings to the flake**

Modify `hosts/lapdog/flake.nix` in the top-level `let` block, before `specialArgs`:

```nix
      opencodeAgentVm = {
        vmName = "opencode-agent-vm";
        stateDir = "/var/lib/opencode-agent-vm";
        controlUser = "dog";
        bridgeName = "opencode-vm0";
        tapName = "opencode-agent0";
        hostAddress = "10.0.101.1";
        guestAddress = "10.0.101.2";
        networkCidr = "10.0.101.0/24";
        prefixLength = 24;
        guestHostname = "opencode-agent-vm";
        guestMac = "02:00:00:00:01:02";
        vcpu = 4;
        mem = 16384;
        opencodePort = 32859;
        hostLocalAddress = "127.0.0.1";
        hostLocalPort = 32860;
        workingDirectory = "/workspace/projects";
        dnsForwardZones = [
          {
            domain = "local.doreto.com.br";
            server = "192.168.0.2";
          }
          {
            domain = "home";
            server = "192.168.0.2";
          }
        ];
        shares = [
          {
            tag = "projects";
            source = "/home/dog/projects";
            mountPoint = "/workspace/projects";
            readOnly = false;
          }
        ];
      };

```

Modify the `specialArgs` attrset:

```diff
       specialArgs = {
-        inherit inputs self;
+        inherit inputs self opencodeAgentVm;
         pkgs-unstable = import nixpkgs {
```

- [x] **Step 2: Enable the host-side service on lapdog**

Modify the function header in `hosts/lapdog/configuration.nix`:

```diff
-{ pkgs, self, ... }:
+{
+  pkgs,
+  self,
+  opencodeAgentVm,
+  ...
+}:
```

Modify the existing dnsmasq settings in `hosts/lapdog/configuration.nix` so `interface` and `listen-address` are lists that can merge with the new module:

```diff
-      interface = "vm0";
+      interface = [ "vm0" ];
       # explicit bind — prevents dnsmasq from
       # falling back to 0.0.0.0:53 if vm0 is late
-      listen-address = "10.0.100.1";
+      listen-address = [ "10.0.100.1" ];
```

Add this block near the existing `lapdog-agent` microVM declaration:

```nix
  dog.services.opencode-agent-vm = opencodeAgentVm // {
    enable = true;
    flake = self;
  };
```

- [x] **Step 3: Run failing host integration evaluation**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.systemd.services.opencode-agent-vm-init.description --raw
```

Expected: FAIL with an attribute-missing error for `opencode-agent-vm-init`, because the host implementation has not been added to the module yet.

- [x] **Step 4: Add host-side implementation helpers**

Modify the `let` block in `modules/nixos/services/opencode-agent-vm.nix` after `dnsForwardZoneType`:

```nix
  dnsServers = (map (zone: "/${zone.domain}/${zone.server}") cfg.dnsForwardZones) ++ cfg.dnsUpstreams;

  startCommand = "${pkgs.systemd}/bin/systemctl start microvm@${cfg.vmName}.service";
  stopCommand = "${pkgs.systemd}/bin/systemctl stop microvm@${cfg.vmName}.service";
  initCommand = "${pkgs.systemd}/bin/systemctl start ${cfg.vmName}-init.service";

  startScript = pkgs.writeShellScriptBin "start-opencode-agent-vm" ''
    set -euo pipefail
    sudo ${initCommand}
    sudo ${startCommand}
    echo "OpenCode agent VM starting: microvm@${cfg.vmName}.service"
    echo ""
    echo "Web UI:              http://${cfg.hostLocalAddress}:${toString cfg.hostLocalPort}"
    echo "SSH:                 opencode-agent-vm-ssh"
    echo "VM public key:       opencode-agent-vm-public-key"
    echo "DNS audit:           journalctl -u dnsmasq -g 'query'"
    echo "Connection audit:    journalctl -k -g '[${cfg.vmName}]'"
    echo "Stop:                stop-opencode-agent-vm"
  '';

  stopScript = pkgs.writeShellScriptBin "stop-opencode-agent-vm" ''
    set -euo pipefail
    sudo ${stopCommand}
    echo "OpenCode agent VM stopped."
  '';

  sshScript = pkgs.writeShellScriptBin "opencode-agent-vm-ssh" ''
    set -euo pipefail
    exec ${pkgs.openssh}/bin/ssh \
      -i ${cfg.stateDir}/ssh/host/host_to_vm \
      -o StrictHostKeyChecking=no \
      -o UserKnownHostsFile=/dev/null \
      ${cfg.guestUser}@${cfg.guestAddress} "$@"
  '';

  publicKeyScript = pkgs.writeShellScriptBin "opencode-agent-vm-public-key" ''
    set -euo pipefail
    if [ ! -f ${cfg.stateDir}/ssh/guest/vm_outbound.pub ]; then
      sudo ${initCommand}
    fi
    ${pkgs.coreutils}/bin/cat ${cfg.stateDir}/ssh/guest/vm_outbound.pub
  '';

  logsScript = pkgs.writeShellScriptBin "opencode-agent-vm-logs" ''
    exec ${pkgs.systemd}/bin/journalctl \
      -u ${cfg.vmName}-init.service \
      -u microvm@${cfg.vmName}.service \
      -u ${cfg.vmName}-proxy.service \
      "$@"
  '';
```

- [x] **Step 5: Add host-side module configuration**

Replace the first `(mkIf cfg.enable { ... })` entry inside the module `config = mkMerge [ ... ]` with:

```nix
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.flake != null;
          message = "dog.services.opencode-agent-vm.flake must be set when the host side is enabled.";
        }
        {
          assertion = cfg.shares != [ ];
          message = "dog.services.opencode-agent-vm.shares must explicitly list every shared host directory.";
        }
      ];

      environment.systemPackages = [
        startScript
        stopScript
        sshScript
        publicKeyScript
        logsScript
      ];

      microvm.vms.${cfg.vmName} = {
        autostart = false;
        flake = cfg.flake;
      };

      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

      networking = {
        networkmanager.unmanaged = mkAfter [
          "interface-name:${cfg.bridgeName}"
          "interface-name:${cfg.tapName}"
        ];

        bridges.${cfg.bridgeName}.interfaces = [ ];

        interfaces.${cfg.bridgeName}.ipv4.addresses = [
          {
            address = cfg.hostAddress;
            prefixLength = cfg.prefixLength;
          }
        ];

        firewall.interfaces.${cfg.bridgeName} = {
          allowedUDPPorts = [ 53 ];
          allowedTCPPorts = [ 53 ];
        };

        firewall.extraForwardRules = ''
          iifname "${cfg.bridgeName}" accept
          oifname "${cfg.bridgeName}" ct state related,established accept
        '';

        nftables.enable = true;
        nftables.tables."${cfg.vmName}-nat" = {
          family = "ip";
          content = ''
            chain postrouting {
              type nat hook postrouting priority srcnat;
              ip saddr ${cfg.networkCidr} oifname != "${cfg.bridgeName}" masquerade
            }
          '';
        };

        nftables.tables."${cfg.vmName}-audit" = mkIf cfg.audit.enable {
          family = "inet";
          content = ''
            chain vm-forward-log {
              type filter hook forward priority filter - 1;
              iifname "${cfg.bridgeName}" ip protocol tcp ct state new log prefix "[${cfg.vmName}] " level info
              iifname "${cfg.bridgeName}" ip protocol udp ct state new log prefix "[${cfg.vmName}] " level info
            }
          '';
        };
      };

      services.dnsmasq = {
        enable = true;
        resolveLocalQueries = cfg.dnsmasqResolveLocalQueries;
        settings = {
          interface = mkAfter [ cfg.bridgeName ];
          listen-address = mkAfter [ cfg.hostAddress ];
          bind-interfaces = true;
          log-queries = cfg.audit.enable;
          no-resolv = true;
          server = mkAfter dnsServers;
        };
      };

      systemd.services."${cfg.vmName}-init" = {
        description = "Initialize OpenCode agent VM state and SSH keys";
        before = [ "microvm@${cfg.vmName}.service" ];
        wantedBy = [ "microvm@${cfg.vmName}.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          set -euo pipefail

          ${pkgs.coreutils}/bin/install -d -m 0755 -o root -g root ${cfg.stateDir}
          ${pkgs.coreutils}/bin/install -d -m 0755 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} ${cfg.stateDir}/home
          ${pkgs.coreutils}/bin/install -d -m 0700 -o ${cfg.controlUser} -g users ${cfg.stateDir}/ssh/host
          ${pkgs.coreutils}/bin/install -d -m 0700 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} ${cfg.stateDir}/ssh/guest

          if [ ! -f ${cfg.stateDir}/ssh/host/host_to_vm ]; then
            ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -C "${cfg.vmName}-host-to-vm" -f ${cfg.stateDir}/ssh/host/host_to_vm
          fi

          host_group="$(${pkgs.coreutils}/bin/id -gn ${cfg.controlUser})"
          ${pkgs.coreutils}/bin/chown ${cfg.controlUser}:"$host_group" ${cfg.stateDir}/ssh/host/host_to_vm ${cfg.stateDir}/ssh/host/host_to_vm.pub
          ${pkgs.coreutils}/bin/chmod 0600 ${cfg.stateDir}/ssh/host/host_to_vm
          ${pkgs.coreutils}/bin/chmod 0644 ${cfg.stateDir}/ssh/host/host_to_vm.pub

          ${pkgs.coreutils}/bin/install -m 0644 -o ${toString cfg.guestUid} -g ${toString cfg.guestGid} \
            ${cfg.stateDir}/ssh/host/host_to_vm.pub \
            ${cfg.stateDir}/ssh/guest/host_to_vm.pub

          if [ ! -f ${cfg.stateDir}/ssh/guest/vm_outbound ]; then
            ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -C "${cfg.vmName}-outbound" -f ${cfg.stateDir}/ssh/guest/vm_outbound
          fi

          ${pkgs.coreutils}/bin/chown ${toString cfg.guestUid}:${toString cfg.guestGid} ${cfg.stateDir}/ssh/guest/vm_outbound ${cfg.stateDir}/ssh/guest/vm_outbound.pub
          ${pkgs.coreutils}/bin/chmod 0600 ${cfg.stateDir}/ssh/guest/vm_outbound
          ${pkgs.coreutils}/bin/chmod 0644 ${cfg.stateDir}/ssh/guest/vm_outbound.pub ${cfg.stateDir}/ssh/guest/host_to_vm.pub
        '';
      };

      systemd.services."microvm-bridge-${cfg.vmName}" = {
        description = "Attach ${cfg.tapName} TAP to ${cfg.bridgeName} bridge for ${cfg.vmName}";
        after = [ "microvm-tap-interfaces@${cfg.vmName}.service" ];
        before = [ "microvm@${cfg.vmName}.service" ];
        partOf = [ "microvm@${cfg.vmName}.service" ];
        wantedBy = [ "microvm@${cfg.vmName}.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          set -euo pipefail
          ${pkgs.iproute2}/bin/ip link set ${cfg.tapName} master ${cfg.bridgeName}
        '';
        preStop = ''
          ${pkgs.iproute2}/bin/ip link set ${cfg.tapName} nomaster 2>/dev/null || true
        '';
      };

      systemd.services."${cfg.vmName}-proxy" = {
        description = "Local-only proxy to OpenCode inside ${cfg.vmName}";
        after = [ "microvm@${cfg.vmName}.service" ];
        partOf = [ "microvm@${cfg.vmName}.service" ];
        wantedBy = [ "microvm@${cfg.vmName}.service" ];
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          RestartSec = "2s";
          ExecStart = "${pkgs.socat}/bin/socat TCP-LISTEN:${toString cfg.hostLocalPort},bind=${cfg.hostLocalAddress},fork,reuseaddr TCP:${cfg.guestAddress}:${toString cfg.opencodePort}";
        };
      };

      security.sudo.extraRules = [
        {
          users = [ cfg.controlUser ];
          commands = [
            {
              command = initCommand;
              options = [ "NOPASSWD" ];
            }
            {
              command = startCommand;
              options = [ "NOPASSWD" ];
            }
            {
              command = stopCommand;
              options = [ "NOPASSWD" ];
            }
          ];
        }
      ];

      services.caddy.virtualHosts.${cfg.caddy.hostName} = mkIf cfg.caddy.enable {
        extraConfig = ''
          request_header X-Forwarded-Host {http.request.host}

          @outpost path /outpost.goauthentik.io/*
          reverse_proxy @outpost ${cfg.caddy.authentikUrl}

          @protected not path /outpost.goauthentik.io/*
          forward_auth @protected ${cfg.caddy.authentikUrl} {
            uri /outpost.goauthentik.io/auth/caddy
            copy_headers X-Authentik-Username
            trusted_proxies private_ranges
          }

          reverse_proxy ${cfg.hostLocalAddress}:${toString cfg.hostLocalPort}
        '';
      };
    })
```

- [x] **Step 6: Verify host integration evaluates**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.systemd.services.opencode-agent-vm-init.description --raw
```

Expected: PASS and prints `Initialize OpenCode agent VM state and SSH keys`.

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.lapdog.config.microvm.vms.opencode-agent-vm.autostart
```

Expected: PASS and prints `false`.

- [x] **Step 7: Commit gate**

Run only if the user explicitly requested commits:

```sh
git add modules/nixos/services/opencode-agent-vm.nix hosts/lapdog/flake.nix hosts/lapdog/configuration.nix
git commit -m "nix: add opencode agent vm host integration"
```

Expected: commit created. If commits were not requested, leave the changes uncommitted.

---

### Task 3: Implement Guest Runtime And Guest Flake Output

**Files:**
- Modify: `modules/nixos/services/opencode-agent-vm.nix`
- Modify: `hosts/lapdog/flake.nix`

**Interfaces:**
- Consumes: `opencodeAgentVm` settings from `hosts/lapdog/flake.nix` and the host-generated state under `/var/lib/opencode-agent-vm/`.
- Produces: `nixosConfigurations.opencode-agent-vm`, guest user `agent`, guest SSH setup, guest `opencode-agent-vm-opencode` systemd service, guest microVM shares, and guest static networking.

- [x] **Step 1: Run failing guest configuration evaluation**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.networking.hostName --raw
```

Expected: FAIL with an attribute-missing error for `opencode-agent-vm`, because the guest flake output does not exist yet.

- [x] **Step 2: Add guest-side implementation helpers**

Modify the `let` block in `modules/nixos/services/opencode-agent-vm.nix` after `logsScript`:

```nix
  persistentHomeShare = {
    proto = "virtiofs";
    tag = "agent-home";
    source = "${cfg.stateDir}/home";
    mountPoint = "/home/${cfg.guestUser}";
  };

  guestSshShare = {
    proto = "virtiofs";
    tag = "agent-ssh";
    source = "${cfg.stateDir}/ssh/guest";
    mountPoint = "/run/opencode-agent-vm/ssh";
    readOnly = true;
  };

  userShareToMicrovmShare = share: {
    proto = "virtiofs";
    inherit (share) tag source mountPoint readOnly;
  };
```

- [x] **Step 3: Add guest-side module configuration**

Replace the second `(mkIf cfg.guest.enable { ... })` entry inside the module `config = mkMerge [ ... ]` with:

```nix
    (mkIf cfg.guest.enable {
      assertions = [
        {
          assertion = cfg.shares != [ ];
          message = "dog.services.opencode-agent-vm.shares must be passed to the guest configuration.";
        }
      ];

      system.stateVersion = "25.05";

      networking = {
        hostName = cfg.guestHostname;
        useNetworkd = true;
        firewall = {
          enable = true;
          allowedTCPPorts = [
            22
            cfg.opencodePort
          ];
        };
      };

      systemd.network = {
        enable = true;
        networks."10-opencode-agent-eth" = {
          matchConfig.Type = "ether";
          networkConfig = {
            Address = "${cfg.guestAddress}/${toString cfg.prefixLength}";
            Gateway = cfg.hostAddress;
            DNS = cfg.hostAddress;
          };
        };
      };

      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "no";
        };
      };

      users.groups.${cfg.guestUser}.gid = cfg.guestGid;
      users.users.${cfg.guestUser} = {
        isNormalUser = true;
        uid = cfg.guestUid;
        group = cfg.guestUser;
        home = "/home/${cfg.guestUser}";
        createHome = false;
        extraGroups = [ "wheel" ];
      };

      security.sudo.wheelNeedsPassword = false;

      environment.systemPackages = with pkgs; [
        curl
        fd
        git
        jq
        neovim
        openssh
        ripgrep
        wget
      ];

      nix.settings.experimental-features = [
        "nix-command"
        "flakes"
      ];

      systemd.tmpfiles.rules = [
        "d /workspace 0755 root root -"
      ];

      systemd.services."${cfg.vmName}-guest-ssh-setup" = {
        description = "Configure SSH material for ${cfg.guestUser} inside ${cfg.vmName}";
        wantedBy = [ "multi-user.target" ];
        before = [
          "sshd.service"
          "${cfg.vmName}-opencode.service"
        ];
        after = [ "local-fs.target" ];
        serviceConfig.Type = "oneshot";
        script = ''
          set -euo pipefail
          ${pkgs.coreutils}/bin/install -d -m 0700 -o ${cfg.guestUser} -g ${cfg.guestUser} /home/${cfg.guestUser}/.ssh
          ${pkgs.coreutils}/bin/install -m 0600 -o ${cfg.guestUser} -g ${cfg.guestUser} \
            /run/opencode-agent-vm/ssh/host_to_vm.pub \
            /home/${cfg.guestUser}/.ssh/authorized_keys
          ${pkgs.coreutils}/bin/ln -sfn /run/opencode-agent-vm/ssh/vm_outbound /home/${cfg.guestUser}/.ssh/id_ed25519
          ${pkgs.coreutils}/bin/ln -sfn /run/opencode-agent-vm/ssh/vm_outbound.pub /home/${cfg.guestUser}/.ssh/id_ed25519.pub
          ${pkgs.coreutils}/bin/chown -h ${cfg.guestUser}:${cfg.guestUser} \
            /home/${cfg.guestUser}/.ssh/id_ed25519 \
            /home/${cfg.guestUser}/.ssh/id_ed25519.pub
        '';
      };

      systemd.services."${cfg.vmName}-opencode" = {
        description = "OpenCode web UI inside ${cfg.vmName}";
        wantedBy = [ "multi-user.target" ];
        wants = [ "network-online.target" ];
        after = [
          "network-online.target"
          "${cfg.vmName}-guest-ssh-setup.service"
        ];
        serviceConfig = {
          Type = "simple";
          User = cfg.guestUser;
          WorkingDirectory = cfg.workingDirectory;
          Environment = [
            "HOME=/home/${cfg.guestUser}"
            "USER=${cfg.guestUser}"
          ];
          ExecStart = "${pkgs.llm-agents.opencode}/bin/opencode serve --hostname ${cfg.guestAddress} --port ${toString cfg.opencodePort}";
          Restart = "on-failure";
          RestartSec = "2s";
        };
      };

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.${cfg.guestUser} = { pkgs, ... }: {
          home = {
            username = cfg.guestUser;
            homeDirectory = "/home/${cfg.guestUser}";
            stateVersion = "25.05";
            packages = with pkgs; [
              llm-agents.opencode
            ];
          };

          targets.genericLinux.enable = true;

          programs = {
            git.enable = true;
            ssh.enable = true;
          };
        };
      };

      systemd.mounts = [
        {
          what = "store";
          where = "/nix/store";
          overrideStrategy = "asDropin";
          unitConfig.DefaultDependencies = false;
        }
      ];

      microvm = {
        hypervisor = "qemu";
        vcpu = cfg.vcpu;
        mem = cfg.mem;
        writableStoreOverlay = "/nix/.rw-store";
        interfaces = [
          {
            type = "tap";
            id = cfg.tapName;
            mac = cfg.guestMac;
          }
        ];
        shares = [
          {
            proto = "virtiofs";
            tag = "ro-store";
            source = "/nix/store";
            mountPoint = "/nix/.ro-store";
          }
          persistentHomeShare
          guestSshShare
        ] ++ map userShareToMicrovmShare cfg.shares;
      };
    })
```

- [x] **Step 4: Add the guest flake output**

Modify `hosts/lapdog/flake.nix` inside `nixosConfigurations = { ... };`, after the existing `lapdog-agent` output:

```nix
        opencode-agent-vm = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            inputs.microvm.nixosModules.microvm
            {
              dog.services.opencode-agent-vm = opencodeAgentVm // {
                guest.enable = true;
              };
            }
          ];
        };
```

- [x] **Step 5: Verify guest configuration evaluates**

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.networking.hostName --raw
```

Expected: PASS and prints `opencode-agent-vm`.

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.systemd.services.opencode-agent-vm-opencode.serviceConfig.User --raw
```

Expected: PASS and prints `agent`.

Run:

```sh
nix eval ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.microvm.shares --json
```

Expected: PASS and JSON includes mount points `/home/agent`, `/run/opencode-agent-vm/ssh`, and `/workspace/projects`.

- [x] **Step 6: Commit gate**

Run only if the user explicitly requested commits:

```sh
git add modules/nixos/services/opencode-agent-vm.nix hosts/lapdog/flake.nix
git commit -m "nix: add opencode agent vm guest runtime"
```

Expected: commit created. If commits were not requested, leave the changes uncommitted.

---

### Task 4: Validate Builds And Operational Commands

**Files:**
- Modify only to fix evaluation or build errors discovered by this task: `modules/nixos/services/opencode-agent-vm.nix`, `hosts/lapdog/flake.nix`, `hosts/lapdog/configuration.nix`

**Interfaces:**
- Consumes: completed host and guest implementation from Tasks 1 through 3.
- Produces: evaluation/build evidence that the host and guest configurations are valid.

- [x] **Step 1: Run flake check**

Run:

```sh
nix flake check ./hosts/lapdog/
```

Expected: PASS. If it fails, fix the exact failing Nix expression and rerun the command until it passes.

- [x] **Step 2: Build the lapdog host system toplevel**

Run:

```sh
nix build ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
```

Expected: PASS and creates or updates `result` to point at the built system closure.

- [x] **Step 3: Build the OpenCode agent VM guest system toplevel**

Run:

```sh
nix build ./hosts/lapdog#nixosConfigurations.opencode-agent-vm.config.system.build.toplevel
```

Expected: PASS and creates or updates `result` to point at the built guest system closure.

- [x] **Step 4: Check formatting-sensitive diff issues**

Run:

```sh
git diff --check
```

Expected: PASS with no output.

- [x] **Step 5: Inspect worktree status**

Run:

```sh
git status --short
```

Expected: modified or added files are limited to the planned files plus pre-existing unrelated untracked files. Do not modify or delete unrelated files such as a pre-existing untracked `opencode.json`.

- [x] **Step 6: Record manual activation instructions for the user**

Report these commands to the user after validation passes:

```sh
nix flake check ./hosts/lapdog/
nix build ./hosts/lapdog#nixosConfigurations.lapdog.config.system.build.toplevel
```

After the user manually switches the host generation, they can run:

```sh
start-opencode-agent-vm
opencode-agent-vm-public-key
opencode-agent-vm-ssh
stop-opencode-agent-vm
```

Expected: the OpenCode UI is printed as `http://127.0.0.1:32860`, the public key command prints the VM outbound public key, SSH logs in as `agent`, and stop shuts down `microvm@opencode-agent-vm.service`.

- [x] **Step 7: Commit gate**

Run only if the user explicitly requested commits:

```sh
git add modules/nixos/default.nix modules/nixos/services/opencode-agent-vm.nix hosts/lapdog/flake.nix hosts/lapdog/configuration.nix
git commit -m "nix: add isolated opencode agent vm"
```

Expected: commit created. If commits were not requested, leave the changes uncommitted.
