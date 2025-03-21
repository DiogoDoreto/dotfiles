{ pkgs, inputs, ... }:

{
  imports = [
    ../../home/_core.nix
    ../../home/_linux.nix
    ./ai.nix
  ];

  home = {
    packages = with pkgs; [
      inkscape
      inputs.home-manager.defaultPackage.x86_64-linux
      nix-tree
      nodePackages.prettier
      nodejs_23
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
        # YouTube improvements
        sponsorblock
        quality-menu
        youtube-upnext
      ];
    };
  };

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
      kokoro-tts = {
        image = "ghcr.io/remsky/kokoro-fastapi-gpu";
        autoStart = true;
        ports = [ "8880:8880" ];
        extraPodmanArgs = [ "--gpus=all" ];
      };
    };
  };
}
