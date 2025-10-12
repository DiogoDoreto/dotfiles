{
  description = "Niri and DankMaterialShell flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    dankMaterialShell = {
      url = "github:AvengeMedia/DankMaterialShell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { niri, dankMaterialShell, ... }:
    {
      overlays = [
        niri.overlays.niri
      ];
      homeModules = [
        dankMaterialShell.homeModules.dankMaterialShell.default
        dankMaterialShell.homeModules.dankMaterialShell.niri
        ./home-niri.nix
      ];
      nixosModules = [
        niri.nixosModules.niri
      ];
    };
}
