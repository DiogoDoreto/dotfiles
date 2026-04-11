# NixOS configuration for the lapdog coding-agent MicroVM.
#
# This is an ephemeral, throwaway VM for running coding agents (Claude Code,
# etc.) in isolation. Nothing persists to disk — the Nix store is read-only
# and all writable state lives in tmpfs. Only the virtiofs/9p shares outlive
# the VM session.
#
# HOW TO RUN:
#   nix run /home/dog/projects/dotfiles/hosts/lapdog#lapdog-agent
#
# This forwards host port 2222 → guest port 22. SSH in with:
#   ssh -p 2222 agent@localhost
#
# The ~/projects directory from the host is available inside the VM at
# /home/agent/projects. SSH authorised keys are read from the host's
# ~/.ssh/authorized_keys so any key you can use on lapdog works here too.
{ pkgs, lib, ... }:
{
  system.stateVersion = "25.05";

  # ── Networking ────────────────────────────────────────────────────────────
  # QEMU user-mode networking (SLIRP) gives the VM a private NAT'd network
  # with internet access out of the box; no host bridge or TAP setup needed.
  networking = {
    hostName = "lapdog-agent";
    useNetworkd = true;
    firewall.enable = false;
  };

  systemd.network = {
    enable = true;
    # Accept whatever address SLIRP hands out via DHCP.
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
      PermitRootLogin = "prohibit-password";
    };
    # Fall back to the host's authorized_keys (mounted via 9p below) so that
    # any SSH key accepted on lapdog also works inside the VM.
    authorizedKeysFiles = lib.mkForce [
      "%h/.ssh/authorized_keys"
      "/run/host-keys/authorized_keys"
    ];
  };

  # ── Users ─────────────────────────────────────────────────────────────────
  # UID 1000 matches the host's "dog" user so 9p-shared files have the right
  # ownership inside the VM (securityModel = "none" skips uid translation).
  users.groups.agent = { };
  users.users.agent = {
    isNormalUser = true;
    uid = 1000;
    group = "agent";
    home = "/home/agent";
    createHome = true;
    extraGroups = [ "wheel" ];
  };

  security.sudo.wheelNeedsPassword = false;

  # ── Packages ──────────────────────────────────────────────────────────────
  # Keep the image small; coding agents need git, shell tools, a runtime or
  # two, and claude-code itself. Add more here as your projects require.
  environment.systemPackages = with pkgs; [
    git
    curl
    wget
    jq
    ripgrep
    fd
    file
    neovim
    python3
    nodejs_24
    # claude-code comes from the llm-agents overlay defined in the flake
    llm-agents.claude-code
  ];

  # Let nix commands work inside the VM (useful for ad-hoc builds).
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # ── Filesystem shares (9p via QEMU virtfs) ────────────────────────────────
  # 9p requires no host daemon — QEMU handles it directly.
  # "cache=mmap" gives reasonable performance for source-code workloads.
  fileSystems."/home/agent/projects" = {
    device = "projects";
    fsType = "9p";
    options = [
      "trans=virtio"
      "version=9p2000.L"
      "cache=mmap"
      "_netdev"
    ];
    neededForBoot = false;
  };

  # Mount the host's ~/.ssh read-only so SSH authorised keys are inherited.
  fileSystems."/run/host-keys" = {
    device = "host-keys";
    fsType = "9p";
    options = [
      "trans=virtio"
      "version=9p2000.L"
      "cache=none"
      "_netdev"
      "ro"
    ];
    neededForBoot = false;
  };

  # ── MicroVM hardware ──────────────────────────────────────────────────────
  microvm = {
    hypervisor = "qemu";

    # 4 vCPUs and 4 GB RAM should be comfortable for most coding-agent tasks.
    # Adjust to taste — the VM is ephemeral so there's no migration cost.
    vcpu = 4;
    mem = 4096;

    # User-mode networking: the VM gets internet access via SLIRP NAT.
    interfaces = [
      {
        type = "user";
        id = "user0";
        mac = "02:00:00:00:00:01";
      }
    ];

    # Host port 2222 → guest port 22 so you can SSH without any host routing.
    forwardPorts = [
      {
        from = "host";
        host.port = 2222;
        guest.port = 22;
      }
    ];

    shares = [
      # Host ~/projects → VM /home/agent/projects (read-write)
      {
        proto = "9p";
        tag = "projects";
        source = "/home/dog/projects";
        mountPoint = "/home/agent/projects";
        securityModel = "none";
      }
      # Host ~/.ssh → VM /run/host-keys (read-only, for authorized_keys)
      {
        proto = "9p";
        tag = "host-keys";
        source = "/home/dog/.ssh";
        mountPoint = "/run/host-keys";
        securityModel = "none";
      }
    ];
  };
}
