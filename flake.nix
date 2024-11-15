{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL/310f8e49a149e4c9ea52f1adf70cdc768ec53f8a";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nixgl, ... }@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ nixgl.overlay ];
      };
    in {
      modules = {
        core = import ./home/_core.nix;
        linux = import ./home/_linux.nix;
      };
      homeConfigurations = {
        home = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home/home.nix ];
          extraSpecialArgs = {
            inherit inputs;
          };
        };
        work = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [ ./home/work.nix ];
          extraSpecialArgs = {
            inherit inputs;
          };
        };
      };
    };
}
