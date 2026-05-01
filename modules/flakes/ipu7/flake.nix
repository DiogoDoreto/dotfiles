{
  description = "Intel IPU7 camera support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import ./overlay.nix) ];
        config.allowUnfree = true;
      };
    in
    {
      overlays.default = import ./overlay.nix;

      nixosModules.default = ./ipu7-module.nix;

      packages.${system} = {
        ipu7-camera-bins = pkgs.ipu7-camera-bins;
        ipu7x-camera-hal = pkgs.ipu7x-camera-hal;
        ipu75xa-camera-hal = pkgs.ipu75xa-camera-hal;
        icamerasrc-ipu7x = pkgs.gst_all_1.icamerasrc-ipu7x;
        icamerasrc-ipu75xa = pkgs.gst_all_1.icamerasrc-ipu75xa;
      };
    };
}
