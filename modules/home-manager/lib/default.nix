{ config, pkgs, ... }:

{
  _module.args = {
    dog-lib = {
      # Create an out-of-store symlink to a path inside the dotfiles repository.
      dotfilesSymlink =
        pathInsideRepo:
        config.lib.file.mkOutOfStoreSymlink (config.dog.dotfilesPath + ("/" + pathInsideRepo));

      bubblewrapAi = args: pkgs.callPackage ./bubblewrap-ai.nix args;
    };
  };
}
