{ config, lib, pkgs, ... }:

with lib;

{
  options.dog = {
    dotfilesPath = mkOption {
      type = types.nullOr types.path;
      description = "Absolute path to the dotfiles repository to be used in
      creating out of store symlinks";
      default = null;
    };
  };
}
