{ pkgs, ... }:

{
  imports = [
    ../../programs
  ];

  home = {
    packages = with pkgs; [
      neovim
      ollama-cuda
    ];

    stateVersion = "24.11";
  };

  programs = {
    starship.settings = {
      username.show_always = true;
    };
  };

  dog.programs = {
    cli-tools.enable = true;
    git.enable = true;
  };
}
