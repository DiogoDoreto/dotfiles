{
  description = "Inspiron 7520 host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, nixos-hardware, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      specialArgs = { inherit inputs; };

      nixos-modules = [
        (import ../../nix-config.nix nixpkgs)
        nixos-hardware.nixosModules.common-cpu-intel
        nixos-hardware.nixosModules.common-gpu-amd
        nixos-hardware.nixosModules.common-pc-laptop
        nixos-hardware.nixosModules.common-pc-laptop-ssd
        ./configuration.nix
      ];
    in
    {
      nixosConfigurations = {
        inspiron7520 = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules;
        };
      };
    };
}
