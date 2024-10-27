{ config, pkgs, lib, inputs, ... }:
{
  imports = [
    ./_core.nix
    ./_linux.nix
  ];

  home = {
    username = "dog";
    homeDirectory = "/home/dog";
  };

  home.packages = with pkgs; [
    ];

  home.file = {
  };

  home.sessionVariables = {
  };

  dog = {
    programs = {
      cli-tools.enable = true;
      emacs.enable = true;
      firefox.enable = true;
      git.enable = true;
      i3.enable = true;
      wezterm.enable = true;
    };
  };
}
