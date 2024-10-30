{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    nur.url = "github:nix-community/NUR";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL/310f8e49a149e4c9ea52f1adf70cdc768ec53f8a";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rofi-material-ocean = {
      url = "github:material-ocean/rofi-Theme/251bd7b81f979b81c8a8382575a4279996e5e243";
      flake = false;
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
