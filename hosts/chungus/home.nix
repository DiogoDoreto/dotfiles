{ pkgs, ... }:

{
  imports = [
    # ./ai.nix
  ];

  home = {
    packages = with pkgs; [
      podman-compose
      pods
      ungoogled-chromium
      nvtopPackages.nvidia
    ];
  };

  programs = {
    bash = {
      initExtra = ''
        if [[ $TERM == "dumb" ]]; then
          export PS1="$ "
        fi
      '';
    };

    starship = {
      enableBashIntegration = false;
    };

    neovim = {
      enable = true;
      withPython3 = false;
      withRuby = false;
    };

    mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        uosc # Feature-rich minimalist proximity-based UI for MPV player
        mpris # allows control of the player using standard media keys
      ];
    };
  };

  dog.presets.linux.enable = true;

  dog.dotfilesPath = /home/dog/p/dotfiles;

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    emacs.enable = true;
    firefox = {
      enable = true;
      plasma-integration = true;
    };
    ghostty.enable = true;
    plasma-fix-taskbar-icons.enable = true;
    opencode = {
      enable = true;
      extraWritablePaths = [
        "~/p/"
      ];
    };
  };

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.podman = {
    enable = true;
    containers = {
    };
  };
}
