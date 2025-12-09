{ pkgs, config, ... }:

{
  imports = [
    ./music.nix
  ];

  home = {
    packages = with pkgs; [
      (callPackage ./bubblewrap-ai.nix {
        package = copilot-cli;
        extraWritablePaths = [
          "~/.copilot/"
          "~/projects/"
        ];
        extraCommandFlags = [
          "--allow-all-tools"
          "--allow-all-paths"
        ];
      })
      (callPackage ./bubblewrap-ai.nix {
        package = crush;
        extraWritablePaths = [
          "~/.config/crush/"
          "~/.local/share/crush/"
          "~/projects/"
        ];
        extraCommandFlags = [
          "--yolo"
        ];
      })
      blender
      clonehero
      kdePackages.kdenlive
      keepassxc
      krita
      libreoffice
      nodejs_24
      obs-studio
      pinentry-emacs
      pods # podman GUI
      ungoogled-chromium
      onedrivegui
    ];
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
      };
    };
  };

  dog.dotfilesPath = /home/dog/projects/dotfiles;

  dog.presets.linux.enable = true;

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    firefox.enable = true;
    niri.enable = true;
    plasma-fix-taskbar-icons.enable = true;
    wezterm.enable = true;

    emacs = {
      enable = true;
      doom.init.lang.common-lisp.enable = true;
    };

    plasma-toggle-tablet-mode = {
      enable = true;
      devices = [
        "AT Translated Set 2 keyboard"
        "ELAN06D5:00 04F3:32B7 Touchpad"
        "TPPS/2 Elan TrackPoint"
        "ThinkPad Extra Buttons"
        "evremap Virtual input for /dev/input/event0"
      ];
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
