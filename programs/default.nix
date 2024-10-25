{ config, lib, pkgs, ... }:

{
  imports = [
    ./firefox.nix
    ./onedrive.nix
    ./podman.nix
  ];
}
