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
        inputs.niri.overlays.niri
        inputs.plasma-toggle-tablet-mode.overlays.${system}.default
        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
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
      home-manager-modules = [
        inputs.dankMaterialShell.homeModules.dankMaterialShell.default
        inputs.dankMaterialShell.homeModules.dankMaterialShell.niri
        ../../modules/home-manager
      ];
      nixos-modules = [
        (import ../../nix-config.nix nixpkgs)
        inputs.niri.nixosModules.niri
        home-manager.nixosModules.home-manager
        {
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.sharedModules = home-manager-modules;
          nixpkgs = { inherit overlays; };
        }
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
        dog = buildHomeFromNixos nixosConfigurations.lapdog.config.users.users.dog ./home.nix;
      };

      nixosConfigurations = {
        lapdog = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga
            ./configuration.nix
          ];
        };
      };
    };
}
