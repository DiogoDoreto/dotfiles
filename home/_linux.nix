{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    "${inputs.home-manager-unstable}/modules/misc/nixgl.nix"
  ];

  targets.genericLinux.enable = true;

  nixGL.packages = inputs.nixgl.packages;
}
