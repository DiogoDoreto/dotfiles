{ config, lib, pkgs, ... }:

{
  imports = [
    ./cli-tools.nix
    ./emacs.nix
    ./firefox.nix
    ./flameshot.nix
    ./git.nix
    ./gromit-mpx.nix
    ./i3.nix
    ./podman.nix
    ./polybar.nix
    ./rofi.nix
    ./wezterm.nix
  ];
}
