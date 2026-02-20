{ pkgs, ... }:

{
  imports = [
    # ./ai.nix
  ];

  home = {
    packages = with pkgs; [
      ungoogled-chromium
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

    neovim.enable = true;

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
    firefox.enable = true;
    ghostty.enable = true;
  };

  services.podman = {
    enable = true;
    containers = {
    };
  };
}
