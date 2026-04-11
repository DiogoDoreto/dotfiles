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
#   nix run /home/dog/projects/dotfiles/hosts/lapdog#lapdog-agent
#
# SSH access (host port 2222 → guest port 22):
#   ssh lapdog-agent         # uses the ~/.ssh/config alias defined in home.nix
#   ssh -p 2222 dog@127.0.0.1
#
# Before first use, authorise your SSH public key:
#   cat ~/.ssh/id_ed25519.pub >> ~/.claude/authorized_keys
#
# The key file lives inside the virtiofs-shared ~/.claude dir, so it is
# persistent across VM runs without any extra sharing.
{ pkgs, lib, ... }:
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
      networkConfig.DHCP = "yes";
    };
  };

  # ── SSH ───────────────────────────────────────────────────────────────────
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
    # Authorised keys live inside the virtiofs-shared ~/.claude directory.
    # Add your public key there once:  cat ~/.ssh/id_ed25519.pub >> ~/.claude/authorized_keys
    authorizedKeysFiles = lib.mkForce [ "%h/.claude/authorized_keys" ];
  };

  # ── Users ─────────────────────────────────────────────────────────────────
  # UID/GID 1000 matches "dog" on the host so virtiofs-shared files have
  # consistent ownership on both sides.
  users.groups.dog = { gid = 1000; };
  users.users.dog = {
    isNormalUser = true;
    uid = 1000;
    group = "dog";
    home = "/home/dog";
    createHome = true;
    extraGroups = [ "wheel" ];
  };

  security.sudo.wheelNeedsPassword = false;

  # ── Packages ──────────────────────────────────────────────────────────────
  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    jq
    ripgrep
    fd
    neovim
    python3
    nodejs_24
    # claude-code via the llm-agents overlay declared in the flake
    llm-agents.claude-code
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
  # Also holds the SSH authorized_keys file for getting into the VM.
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

  # ── MicroVM hardware ──────────────────────────────────────────────────────
  microvm = {
    hypervisor = "qemu";
    vcpu = 4;
    mem = 4096;

    # User-mode networking: internet via SLIRP NAT, no host config needed.
    interfaces = [
      {
        type = "user";
        id = "user0";
        mac = "02:00:00:00:00:01";
      }
    ];

    # Host port 2222 → guest port 22 for SSH access from the host.
    forwardPorts = [
      {
        from = "host";
        host.port = 2222;
        guest.port = 22;
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
