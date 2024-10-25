{ config, lib, pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./firefox.nix
    ./onedrive.nix
    ./podman.nix
  ];
}
