{ pkgs, inputs, ... }:

{
  imports = [
    ../../home/_core.nix
    ../../home/_linux.nix
  ];

  home = {
    packages = with pkgs; [
      inputs.home-manager.defaultPackage.x86_64-linux
      nodejs_23
      ungoogled-chromium
      keepassxc
      onedrivegui
      qbittorrent
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

    neovim.enable = true;
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    ghostty.enable = true;
    podman.enable = true;
  };
}
