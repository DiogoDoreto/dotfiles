{
  description = "KWtype - Virtual keyboard input tool for KDE Wayland";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      kwtype = pkgs.callPackage ./package.nix { };
    in
    {
      overlays.default = final: prev: {
        inherit kwtype;
      };

      packages.${system} = {
        inherit kwtype;
        default = kwtype;
      };
    };
}
