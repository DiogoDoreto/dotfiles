# Keep ./README.md up to date when changing this module's behavior.
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.services.agent-vm;
in
{
  imports = [ ./options.nix ];

  system.stateVersion = "25.05";

  networking = {
    hostName = cfg.guestHostname;
    useNetworkd = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22
      ]
      ++ optional cfg.opencode.enable cfg.opencodePort
      ++ optional cfg.paseo.enable cfg.paseo.guestPort;
    };
  };

  systemd.network = {
    enable = true;
    networks."10-agent-vm-eth" = {
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
    hostKeys = [
      {
        type = "ed25519";
        path = "/var/lib/ssh/ssh_host_ed25519_key";
      }
      {
        type = "rsa";
        bits = 4096;
        path = "/var/lib/ssh/ssh_host_rsa_key";
      }
    ];
  };

  services.tailscale.enable = cfg.guest.tailscale.enable;

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
  security.pki.certificateFiles = [ ../../../../hosts/mini/home-caddy.crt ];

  environment.systemPackages = with pkgs; [
    curl
    fd
    gh
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

  systemd.services."${cfg.vmName}-guest-ssh-setup" = {
    description = "Configure SSH material for ${cfg.guestUser} inside ${cfg.vmName}";
    wantedBy = [ "multi-user.target" ];
    before = [
      "sshd.service"
    ]
    ++ optional cfg.opencode.enable "${cfg.vmName}-opencode.service"
    ++ optional cfg.paseo.enable "paseo.service";
    after = [ "local-fs.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      set -euo pipefail
      ${pkgs.coreutils}/bin/install -d -m 0700 -o ${cfg.guestUser} -g ${cfg.guestUser} /home/${cfg.guestUser}/.ssh
      ${pkgs.coreutils}/bin/install -m 0600 -o ${cfg.guestUser} -g ${cfg.guestUser} \
        /run/agent-vm/ssh/host_to_vm.pub \
        /home/${cfg.guestUser}/.ssh/authorized_keys
      ${pkgs.coreutils}/bin/ln -sfn /run/agent-vm/ssh/vm_outbound /home/${cfg.guestUser}/.ssh/id_ed25519
      ${pkgs.coreutils}/bin/ln -sfn /run/agent-vm/ssh/vm_outbound.pub /home/${cfg.guestUser}/.ssh/id_ed25519.pub
      ${pkgs.coreutils}/bin/chown -h ${cfg.guestUser}:${cfg.guestUser} \
        /home/${cfg.guestUser}/.ssh/id_ed25519 \
        /home/${cfg.guestUser}/.ssh/id_ed25519.pub
    '';
  };

  systemd.services."${cfg.vmName}-opencode" = mkIf cfg.opencode.enable {
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
      ExecStart = "${pkgs.bash}/bin/bash -lc 'exec ${pkgs.llm-agents.opencode}/bin/opencode serve --hostname ${cfg.guestAddress} --port ${toString cfg.opencodePort}'";
      Restart = "on-failure";
      RestartSec = "2s";
    };
  };

  systemd.services.paseo = mkIf cfg.paseo.enable {
    description = "Paseo agent control surface inside ${cfg.vmName}";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [
      "network-online.target"
      "${cfg.vmName}-guest-ssh-setup.service"
    ];
    environment = {
      PATH = mkForce (
        concatStringsSep ":" [
          "/home/${cfg.guestUser}/.nix-profile/bin"
          "/home/${cfg.guestUser}/.local/state/nix/profile/bin"
          "/etc/profiles/per-user/${cfg.guestUser}/bin"
          "/run/current-system/sw/bin"
          "/run/wrappers/bin"
          "/nix/var/nix/profiles/default/bin"
        ]
      );
      PASEO_HOME = "/home/${cfg.guestUser}/.paseo";
      PASEO_HOSTNAMES = cfg.paseo.hostName;
      PASEO_LISTEN = "${cfg.guestAddress}:${toString cfg.paseo.guestPort}";
      PASEO_TRUSTED_PROXIES = cfg.hostAddress;
      PASEO_WEB_UI_ENABLED = "true";
    };
    serviceConfig = {
      Type = "simple";
      User = cfg.guestUser;
      WorkingDirectory = cfg.workingDirectory;
      ExecStart = "${pkgs.paseo}/bin/paseo-server --no-relay";
      Restart = "on-failure";
      RestartSec = "5s";
      KillSignal = "SIGTERM";
      TimeoutStopSec = 15;
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${cfg.guestUser} = { pkgs, ... }: {
      dog.dotfilesPath = "/home/${cfg.guestUser}/projects/dotfiles";
      home = {
        username = cfg.guestUser;
        homeDirectory = "/home/${cfg.guestUser}";
        stateVersion = "25.05";
        packages =
          with pkgs;
          optional cfg.opencode.enable llm-agents.opencode ++ optional cfg.paseo.enable paseo;
      };
      targets.genericLinux.enable = true;
      dog.programs = {
        agent-skills.enable = true;
        cli-tools.enable = true;
        git.enable = true;
        forgejo-cli = {
          enable = true;
          host = "https://git.local.doreto.com.br";
        };
        playwright-cli = {
          enable = true;
          installAgentsSkill = true;
        };
      };
      programs.ssh.enable = true;
    };
  };

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
    volumes = [
      {
        image = "${cfg.stateDir}/rw-store.img";
        label = "nix-store";
        mountPoint = "/nix/.rw-store";
        size = 65536;
      }
    ];
    shares = [
      {
        proto = "virtiofs";
        tag = "ro-store";
        source = "/nix/store";
        mountPoint = "/nix/.ro-store";
        readOnly = true;
      }
      {
        proto = "virtiofs";
        tag = "agent-home";
        source = "${cfg.stateDir}/home";
        mountPoint = "/home/${cfg.guestUser}";
      }
      {
        proto = "virtiofs";
        tag = "agent-var-lib";
        source = "${cfg.stateDir}/var/lib";
        mountPoint = "/var/lib";
      }
      {
        proto = "virtiofs";
        tag = "agent-var-log";
        source = "${cfg.stateDir}/var/log";
        mountPoint = "/var/log";
      }
      {
        proto = "virtiofs";
        tag = "agent-ssh";
        source = "${cfg.stateDir}/ssh/guest";
        mountPoint = "/run/agent-vm/ssh";
        readOnly = true;
      }
    ]
    ++ map (share: {
      proto = "virtiofs";
      inherit (share)
        tag
        source
        mountPoint
        readOnly
        ;
    }) cfg.shares;
  };
}
