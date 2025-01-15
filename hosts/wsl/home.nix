{ pkgs, ... }:

{
  imports = [
    ../../programs
  ];

  home = {
    packages = with pkgs; [
      neovim
    ];

    stateVersion = "24.11";
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
  };
}
