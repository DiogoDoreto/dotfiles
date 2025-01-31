{ pkgs, ... }:

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
      bottom
      nodePackages.prettier
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
      settings = {
        username.show_always = true;
      };
    };

    neovim.enable = true;
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    ghostty.enable = true;
  };
}
