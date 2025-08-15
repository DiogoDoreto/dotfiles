{
  description = "Lapdog host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    plasma-toggle-tablet-mode = {
      url = "../../scripts/plasma-toggle-tablet-mode";
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
      nixos-hardware,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.nur.overlays.default
        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
          inherit (inputs.plasma-toggle-tablet-mode.packages.${system}) plasma-toggle-tablet-mode;
        })
      ];
      pkgs-config = {
        inherit system overlays;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      pkgs = import nixpkgs pkgs-config;
      specialArgs = {
        inherit inputs;
        pkgs-unstable = pkgs;
      };
      buildHomeFromNixos =
        user: entryModule:
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = specialArgs;
          modules = [
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
        dog = buildHomeFromNixos nixosConfigurations.lapdog.config.users.users.dog ./home.nix;
      };

      nixosConfigurations = {
        lapdog = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga
            (import ../../nix-config.nix nixpkgs)
            ./configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = specialArgs;
              nixpkgs = { inherit overlays; };
            }
          ];
        };
      };
    };
}
