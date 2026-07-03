{
  pkgs,
  config,
  dog-lib,
  ...
}:

let
  inherit (dog-lib) dotfilesSymlink;
  inherit (pkgs.lib) filterAttrs listToAttrs nameValuePair;

  agentSkillNames = builtins.attrNames (
    filterAttrs (_: type: type == "directory") (builtins.readDir ../../.config/agents/skills)
  );
  agentSkillFiles = listToAttrs (
    map (
      name:
      nameValuePair ".agents/skills/${name}" { source = dotfilesSymlink ".config/agents/skills/${name}"; }
    ) agentSkillNames
  );
in
{
  imports = [
    ./music.nix
  ];

  home = {
    packages = with pkgs; [
      # (dog-lib.bubblewrapAi {
      #   # useful to verify bwrap script
      #   package = fish;
      #   wrappedBinName = "ai-shell";
      # })
      # blender
      # clonehero
      # freecad
      imagemagick
      # llm-agents.agent-browser
      kdePackages.kdenlive
      keepassxc
      krita
      kwtype
      libreoffice
      obs-studio
      pinentry-emacs
      podman-compose
      pods # podman GUI
      # pureref
      sshfs
      ungoogled-chromium
      onedrivegui
      wl-clipboard
    ];
  };

  xdg.autostart = {
    enable = true;
    entries = [ "${pkgs.onedrivegui}/share/applications/OneDriveGUI.desktop" ];
  };

  home.file = agentSkillFiles;

  programs = {
    mpv = {
      enable = true;
      package = pkgs.mpv.override {
        youtubeSupport = false;
        scripts = with pkgs.mpvScripts; [
          uosc # Feature-rich minimalist proximity-based UI for MPV player
          mpris # allows control of the player using standard media keys
        ];
      };
      bindings = {
        "Alt+RIGHT" = "cycle_values video-rotate 90 180 270 0";
        "Alt+LEFT" = "cycle_values video-rotate 0 270 180 90";
      };
    };

    neovim = {
      enable = true;
      withPython3 = false;
      withRuby = false;
    };

    nh = {
      enable = true;
      flake = config.dog.dotfilesPath + "/hosts/lapdog";
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings = {
        "*" = {
          ForwardAgent = false;
          AddKeysToAgent = "no";
          Compression = false;
          ServerAliveInterval = 0;
          ServerAliveCountMax = 3;
          HashKnownHosts = false;
          UserKnownHostsFile = "~/.ssh/known_hosts";
          ControlMaster = "auto";
          ControlPath = "~/.ssh/cm-%r@%n:%p";
          ControlPersist = "10m";
        };
        "dogdot" = {
          HostName = "192.168.0.2";
          Port = 17098;
          User = "root";
          AddKeysToAgent = "yes";
          IdentityFile = "~/.ssh/id_ed25519_dogdot";
        };
        "dogdot-ts" = {
          HostName = "100.117.142.110";
          Port = 17098;
          User = "root";
          AddKeysToAgent = "yes";
          IdentityFile = "~/.ssh/id_ed25519_dogdot";
        };
        "chungus" = {
          HostName = "192.168.0.3";
          User = "dog";
          AddKeysToAgent = "yes";
          IdentityFile = "~/.ssh/id_ed25519_dogdot";
        };
        "opencode-mini" = {
          HostName = "10.0.101.2";
          User = "agent";
          ProxyJump = "dogdot";
          AddKeysToAgent = "yes";
          IdentityFile = "~/.ssh/opencode-mini";
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
    ghostty.enable = true;
    git.enable = true;
    websearch.enable = true;
    firefox = {
      enable = true;
      plasma-integration = true;
    };
    plasma-fix-taskbar-icons.enable = true;

    opencode = {
      enable = true;
      extraWritablePaths = [
        "~/projects/"
      ];
    };

    forgejo-cli = {
      enable = true;
      host = "https://git.local.doreto.com.br";
    };

    playwright-cli = {
      enable = true;
      installAgentsSkill = true;
    };

    emacs = {
      enable = true;

      doom.init = {
        app = {
          rss.enable = true;
        };
        term = {
          ghostel.enable = true;
          vterm.enable = false;
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

  services.handy.enable = true;

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.nextcloud-client = {
    enable = true;
    startInBackground = true;
  };
}
