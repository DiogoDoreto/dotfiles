{ config, lib, pkgs, ... }:

{
  imports = [
    ./cli-tools.nix
    ./emacs.nix
    ./firefox.nix
    ./git.nix
    ./i3.nix
    ./onedrive.nix
    ./podman.nix
    ./wezterm.nix
  ];
}
