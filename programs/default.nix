{ config, lib, pkgs, ... }:

{
  imports = [
    ./cli-tools.nix
    ./emacs.nix
    ./firefox.nix
    ./git.nix
    ./gromit-mpx.nix
    ./i3.nix
    ./onedrive.nix
    ./podman.nix
    ./polybar.nix
    ./rofi.nix
    ./wezterm.nix
  ];
}
