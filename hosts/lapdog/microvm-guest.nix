# NixOS configuration for the lapdog coding-agent MicroVM.
#
# This is an ephemeral, throwaway VM for running coding agents (Claude Code,
# etc.) in isolation. Nothing persists to disk — the Nix store is read-only
# and all writable state lives in tmpfs. Only the virtiofs-shared paths
# survive the session (projects, ~/.claude, ~/.claude.json).
#
# HOW TO RUN:
#   start-agent-vm          # convenience wrapper in dog's $PATH
#   # or directly:
#   sudo systemctl start microvm@lapdog-agent
#
# SSH access (VM is at 10.0.100.2 on the vm0 bridge):
#   ssh lapdog-agent         # uses the ~/.ssh/config alias defined in home.nix
#   ssh dog@10.0.100.2
{ pkgs, ... }:
{
  system.stateVersion = "25.05";

  # ── Networking ────────────────────────────────────────────────────────────
  # QEMU user-mode networking (SLIRP): internet access via NAT, no host
  # bridge or TAP setup required.
  networking = {
    hostName = "lapdog-agent";
    useNetworkd = true;
    firewall.enable = false;
  };

  systemd.network = {
    enable = true;
    networks."10-microvm-eth" = {
      matchConfig.Type = "ether";
      networkConfig = {
        Address = "10.0.100.2/24";
        Gateway = "10.0.100.1";
        # Use the host's dnsmasq instance on the bridge; it logs all queries.
        DNS = "10.0.100.1";
      };
    };
  };

  # ── SSH ───────────────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # ── Users ─────────────────────────────────────────────────────────────────
  # UID/GID 1000 matches "dog" on the host so virtiofs-shared files have
  # consistent ownership on both sides.
  users.groups.dog = {
    gid = 1000;
  };
  users.users.dog = {
    isNormalUser = true;
    uid = 1000;
    group = "dog";
    home = "/home/dog";
    createHome = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFzvUuNy14x6avfx0mYrG3txTKgQZbTADajlZ7Sjk1bz dog@lapdog"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  # ── Packages ──────────────────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    curl
    wget
    neovim
  ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # ── Filesystem mounts (virtiofs) ──────────────────────────────────────────
  # virtiofs tags must match the `tag` fields in microvm.shares below.

  # ~/projects — the main working area (read-write)
  fileSystems."/home/dog/projects" = {
    device = "projects";
    fsType = "virtiofs";
    options = [ "defaults" ];
    neededForBoot = false;
  };

  # ~/.claude — persistent agent context directory (read-write).
  fileSystems."/home/dog/.claude" = {
    device = "claude-dir";
    fsType = "virtiofs";
    options = [ "defaults" ];
    neededForBoot = false;
  };

  # /run/claude-share — staging dir on the host containing a bind-mounted
  # copy of ~/.claude.json (auth token / settings).  A tmpfiles symlink
  # in the guest wires it to the expected ~/.claude.json path.
  fileSystems."/run/claude-share" = {
    device = "claude-share";
    fsType = "virtiofs";
    options = [ "defaults" ];
    neededForBoot = false;
  };

  # Create ~/.claude.json as a symlink into the shared staging dir.
  # tmpfiles runs after local-fs.target, so the virtiofs mounts are up.
  systemd.tmpfiles.rules = [
    "L+ /home/dog/.claude.json - - - - /run/claude-share/.claude.json"
  ];

  # ── Home Manager ─────────────────────────────────────────────────────────
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.dog = ./microvm-home.nix;
  };

  # ── MicroVM hardware ──────────────────────────────────────────────────────
  microvm = {
    hypervisor = "qemu";
    vcpu = 4;
    mem = 4096;

    # TAP networking: traffic goes through the host kernel, so nftables
    # rules on the host can filter and log it (unlike SLIRP which bypasses
    # the kernel entirely).  The host sets up bridge vm0 + NAT; see
    # configuration.nix for the nftables and dnsmasq setup.
    interfaces = [
      {
        type = "tap";
        id = "vm-agent0";
        mac = "02:00:00:00:00:01";
      }
    ];

    shares = [
      # ~/projects — main code working directory
      {
        proto = "virtiofs";
        tag = "projects";
        source = "/home/dog/projects";
        mountPoint = "/home/dog/projects";
      }
      # ~/.claude — agent context (memories, settings) + SSH authorized_keys
      {
        proto = "virtiofs";
        tag = "claude-dir";
        source = "/home/dog/.claude";
        mountPoint = "/home/dog/.claude";
      }
      # ~/.claude.json — auth token / API settings (via host bind mount;
      # see configuration.nix systemd.services.lapdog-agent-bind-claude-json)
      {
        proto = "virtiofs";
        tag = "claude-share";
        source = "/var/lib/lapdog-agent/claude-share";
        mountPoint = "/run/claude-share";
      }
    ];
  };
}
