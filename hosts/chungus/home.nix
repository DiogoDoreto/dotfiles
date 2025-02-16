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
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    ghostty.enable = true;
  };
}
