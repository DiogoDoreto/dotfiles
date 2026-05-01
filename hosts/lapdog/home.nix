{
  pkgs,
  config,
  dog-lib,
  ...
}:

let
  inherit (dog-lib) dotfilesSymlink;
in
{
  imports = [
    ./music.nix
  ];

  home = {
    packages = with pkgs; [
      # Start the ephemeral coding-agent MicroVM and print SSH instructions.
      # Usage: start-agent-vm [extra qemu args…]
      (pkgs.writeShellScriptBin "start-agent-vm" ''
        set -euo pipefail
        mkdir -p /home/dog/.local/share/lapdog-agent/home
        sudo ${pkgs.systemd}/bin/systemctl start microvm@lapdog-agent.service
        echo "VM starting (systemctl start microvm@lapdog-agent)."
        echo ""
        echo "SSH in once it's ready:"
        echo "  ssh lapdog-agent"
        echo ""
        echo "Audit DNS queries:    journalctl -u dnsmasq -g 'query'"
        echo "Audit connections:    journalctl -k -g '\[vm-agent\]'"
        echo "Stop:                 stop-agent-vm"
      '')
      (pkgs.writeShellScriptBin "stop-agent-vm" ''
        sudo ${pkgs.systemd}/bin/systemctl stop microvm@lapdog-agent.service
        echo "VM stopped."
      '')
      # (dog-lib.bubblewrapAi {
      #   # useful to verify bwrap script
      #   package = fish;
      #   wrappedBinName = "ai-shell";
      # })
      # blender
      # clonehero
      # freecad
      handy
      imagemagick
      # llm-agents.agent-browser
      kdePackages.kdenlive
      keepassxc
      krita
      kwtype
      libreoffice
      nodejs_24
      obs-studio
      pinentry-emacs
      podman-compose
      pods # podman GUI
      pureref
      sshfs
      ungoogled-chromium
      onedrivegui
      forgejo-cli
    ];
  };

  xdg.autostart = {
    enable = true;
    entries = [ "${pkgs.onedrivegui}/share/applications/OneDriveGUI.desktop" ];
  };

  home.file = {
    ".agents".source = dotfilesSymlink ".config/agents";
  };

  programs = {
    mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        uosc # Feature-rich minimalist proximity-based UI for MPV player
        mpris # allows control of the player using standard media keys
        # YouTube improvements
        sponsorblock
        quality-menu
        youtube-upnext
      ];
      bindings = {
        "Alt+RIGHT" = "cycle_values video-rotate 90 180 270 0";
        "Alt+LEFT" = "cycle_values video-rotate 0 270 180 90";
      };
    };

    neovim.enable = true;

    nh = {
      enable = true;
      flake = config.dog.dotfilesPath + "/hosts/lapdog";
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks = {
        "*" = {
          forwardAgent = false;
          addKeysToAgent = "no";
          compression = false;
          serverAliveInterval = 0;
          serverAliveCountMax = 3;
          hashKnownHosts = false;
          userKnownHostsFile = "~/.ssh/known_hosts";
          controlMaster = "no";
          controlPath = "~/.ssh/master-%r@%n:%p";
          controlPersist = "no";
        };
        "dogdot" = {
          hostname = "192.168.0.2";
          port = 17098;
          user = "root";
          addKeysToAgent = "yes";
          identityFile = "~/.ssh/id_ed25519_dogdot";
        };
        "dogdot-ts" = {
          hostname = "100.117.142.110";
          port = 17098;
          user = "root";
          addKeysToAgent = "yes";
          identityFile = "~/.ssh/id_ed25519_dogdot";
        };
        "chungus" = {
          hostname = "192.168.0.3";
          user = "dog";
          addKeysToAgent = "yes";
          identityFile = "~/.ssh/id_ed25519_dogdot";
        };
        # Ephemeral coding-agent MicroVM (started with `nix run
        # ~/projects/dotfiles/hosts/lapdog#lapdog-agent`).
        # The VM is throwaway — skip host-key verification since the key
        # regenerates each run.
        # Coding-agent MicroVM on the vm0 bridge.
        # Start with: start-agent-vm
        "lapdog-agent" = {
          hostname = "10.0.100.2";
          user = "dog";
          identityFile = "~/.ssh/id_ed25519_dogdot";
          # Host key changes every run (ephemeral VM) — skip verification.
          extraOptions = {
            StrictHostKeyChecking = "no";
            UserKnownHostsFile = "/dev/null";
          };
        };
      };
    };
  };

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.presets.linux.enable = true;

  dog.services.auto-commit = {
    enable = true;
    watchedDirectories = [ "/home/dog/Nextcloud/Notes" ];
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    firefox.enable = true;
    niri.enable = true;
    plasma-fix-taskbar-icons.enable = true;
    wezterm.enable = true;

    claude-code = {
      enable = true;
      bubblewrap = {
        enable = true;
        extraWritablePaths = [
          "~/projects/"
          "/run/user/1000/agent-browser"
        ];
      };
    };

    opencode = {
      enable = true;
      extraWritablePaths = [
        "~/projects/"
      ];
    };

    pi = {
      enable = true;
      extraWritablePaths = [
        "~/projects/"
      ];
    };

    emacs = {
      enable = true;
      ghostel.enable = true;

      doom.init = {
        app = {
          rss.enable = true;
        };
        lang = {
          # common-lisp.enable = true;
          # elixir = {
          #   enable = true;
          #   flags = [
          #     "+lsp"
          #     "+tree-sitter"
          #   ];
          # };
          # elixir-extra.enable = true;
          # scheme = {
          #   enable = true;
          #   flags = [ "+guile" ];
          # };
        };
      };
    };
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentry = {
      package = pkgs.pinentry-emacs;
      program = "pinentry";
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };
}
