{
  description = "Chungus host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.nur.overlays.default
        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
          cudaSupport = true;
        };
      };
      specialArgs = {
        inherit inputs;
        pkgs-unstable = pkgs;
      };

      home-manager-modules = [
        ../../modules/home-manager
      ];

      nixos-modules = [
        (import ../../nix-config.nix nixpkgs)
        home-manager.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.sharedModules = home-manager-modules;
          nixpkgs = { inherit overlays; };
        }
        ./configuration.nix
      ];

      buildHomeFromNixos =
        user: entryModule:
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = home-manager-modules ++ [
            {
              home = {
                username = user.name;
                homeDirectory = user.home;
              };
            }
            entryModule
          ];
        };
    in
    rec {
      homeConfigurations = {
        dog = buildHomeFromNixos nixosConfigurations.chungus.config.users.users.dog ./home.nix;
      };
      nixosConfigurations = {
        chungus = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules;
        };
      };
    };
}
