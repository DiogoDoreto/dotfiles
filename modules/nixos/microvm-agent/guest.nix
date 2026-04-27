# Base NixOS module for any microvm coding-agent guest (agent or router role).
#
# Provides the common skeleton: users, SSH, nix settings, virtiofs shares, and
# the microvm hardware block.  Import this module for every VM; import
# router.nix *in addition* when building the router VM.
#
# See README.md in this directory for the full architecture description.
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.microvm-agent;
in
{
  options.dog.microvm-agent = {
    hostname = mkOption {
      type = types.str;
      description = "VM hostname, e.g. \"agent-work\".";
    };

    guestUser = mkOption {
      type = types.str;
      default = "dog";
      description = "Username created inside the guest.";
    };

    uid = mkOption {
      type = types.int;
      default = 1000;
      description = "UID/GID for the guest user. Must match the host user so virtiofs-shared files have consistent ownership.";
    };

    sshAuthorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "SSH public keys allowed to log in as guestUser.";
    };

    shares = mkOption {
      type = types.listOf (types.submodule {
        options = {
          tag       = mkOption { type = types.str; };
          source    = mkOption { type = types.str; description = "Absolute path on the HOST."; };
          mountPoint = mkOption { type = types.str; description = "Absolute path inside the guest."; };
          readOnly  = mkOption { type = types.bool; default = false; };
        };
      });
      default = [ ];
      description = "Extra virtiofs shares beyond the mandatory ro-store. Order matters: shares are mounted in sequence during guest boot.";
    };

    net = {
      backend = mkOption {
        type = types.enum [ "vde" "tap" ];
        default = "vde";
        description = ''
          "vde" — userspace VDE2 switch; no host privileges required (Ubuntu default).
          "tap" — kernel TAP device attached to a host bridge (lapdog/NixOS default).
        '';
      };

      address = mkOption {
        type = types.str;
        description = "Static IP with CIDR for the internal NIC, e.g. \"10.0.0.2/24\".";
      };

      gateway = mkOption {
        type = types.str;
        default = "10.0.0.1";
        description = "Default gateway inside the guest (= the router VM's address).";
      };

      dns = mkOption {
        type = types.str;
        default = "10.0.0.1";
        description = "DNS server inside the guest (= the router VM's dnsmasq).";
      };

      mac = mkOption {
        type = types.str;
        description = "MAC address for the primary internal NIC. Must be unique across all VMs.";
      };

      # vde-specific
      vdeSocket = mkOption {
        type = types.str;
        default = "/run/user/1000/agent-vm-net.sock";
        description = "Path to the vde_switch socket on the host (created by the agent-vm-switch user service).";
      };

      # tap-specific (lapdog / NixOS hosts)
      tapId = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "TAP device name on the host. Required when net.backend = \"tap\".";
      };
    };
  };

  config = {
    networking.hostName = cfg.hostname;
    networking.useNetworkd = true;
    networking.firewall.enable = false;

    # Rename the internal NIC to a predictable name so nftables and dnsmasq
    # can reference it symbolically rather than relying on boot-order naming.
    systemd.network.links."10-vm-int" = {
      matchConfig.MACAddress = cfg.net.mac;
      linkConfig.Name = "vm-int";
    };

    systemd.network = {
      enable = true;
      networks."10-vm-int" = {
        matchConfig.Name = "vm-int";
        networkConfig = {
          Address              = cfg.net.address;
          Gateway              = cfg.net.gateway;
          DNS                  = cfg.net.dns;
          LinkLocalAddressing  = "no";
          DHCP                 = "no";
        };
      };
    };

    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin        = "no";
      };
    };

    users.groups.${cfg.guestUser}.gid = cfg.uid;
    users.users.${cfg.guestUser} = {
      isNormalUser = true;
      uid          = cfg.uid;
      group        = cfg.guestUser;
      home         = "/home/${cfg.guestUser}";
      # Home directory is provided by the "agent-home" virtiofs share; do not
      # create it here (the share must be mounted first, which happens at boot).
      createHome   = false;
      extraGroups  = [ "wheel" ];
      openssh.authorizedKeys.keys = cfg.sshAuthorizedKeys;
    };

    security.sudo.wheelNeedsPassword = false;

    environment.systemPackages = with pkgs; [ curl wget git ];

    nix.settings.experimental-features = [ "nix-command" "flakes" ];

    # Only needed when a "claude-share" virtiofs tag is present (the host
    # bind-mounts ~/.claude.json into a staging directory so the single file
    # can be shared).  By default each VM carries its own .claude.json inside
    # its persistent home and no symlink is required.
    systemd.tmpfiles.rules =
      mkIf (any (s: s.tag == "claude-share") cfg.shares) [
        "L+ /home/${cfg.guestUser}/.claude.json - - - - /run/claude-share/.claude.json"
      ];

    microvm = {
      hypervisor          = "qemu";
      writableStoreOverlay = "/nix/.rw-store";

      shares = [
        # Host /nix/store shared read-only; writable overlay above handles
        # writes from nix-daemon and home-manager activation inside the guest.
        {
          proto      = "virtiofs";
          tag        = "ro-store";
          source     = "/nix/store";
          mountPoint = "/nix/.ro-store";
        }
      ] ++ map (s: {
        proto      = "virtiofs";
        tag        = s.tag;
        source     = s.source;
        mountPoint = s.mountPoint;
        readOnly   = s.readOnly;
      }) cfg.shares;

      # TAP backend: use the microvm.interfaces option (handled natively).
      # VDE backend: microvm.nix removed first-class VDE support; inject the
      # NIC directly via extraArgs so QEMU still creates the virtio-net device.
      interfaces = mkIf (cfg.net.backend == "tap") [
        {
          type = "tap";
          id   = assert cfg.net.tapId != null; cfg.net.tapId;
          mac  = cfg.net.mac;
        }
      ];

      qemu.extraArgs = mkIf (cfg.net.backend == "vde") [
        "-netdev" "vde,id=net0,sock=${cfg.net.vdeSocket}"
        "-device" "virtio-net-pci,netdev=net0,mac=${cfg.net.mac}"
      ];
    };

    # Without this, systemd tries to unmount /nix/store during shutdown but
    # umount lives in /nix/store — deadlock.  Drop the DefaultDependencies so
    # the mount unit is not torn down before other units have stopped.
    systemd.mounts = [
      {
        what              = "store";
        where             = "/nix/store";
        overrideStrategy  = "asDropin";
        unitConfig.DefaultDependencies = false;
      }
    ];

    system.stateVersion = "25.05";
  };
}
