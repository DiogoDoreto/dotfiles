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
