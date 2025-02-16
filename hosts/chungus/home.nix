{ pkgs, inputs, ... }:

{
  imports = [
    ../../home/_core.nix
    ../../home/_linux.nix
    ./ai.nix
  ];

  home = {
    packages = with pkgs; [
      nodejs_23
      nix-tree
      nodePackages.prettier
      ungoogled-chromium
      inputs.home-manager.defaultPackage.x86_64-linux
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
}
