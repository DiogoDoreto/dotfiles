{
  description = "Mini host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    authentik-nix.url = "github:nix-community/authentik-nix";
    llm-agents.url = "github:numtide/llm-agents.nix";
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
    # plasma-manager = {
    #   url = "github:nix-community/plasma-manager";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.home-manager.follows = "home-manager";
    # };
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
        inputs.llm-agents.overlays.default
        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
        })
        # TODO remove when https://github.com/NixOS/nixpkgs/pull/494140 is available in nixos-unstable
        (final: prev: {
          calibre-web = prev.calibre-web.overrideAttrs (old: {
            version = "0.6.26-unstable-2026-03-01";
            src = prev.fetchFromGitHub {
              owner = "janeczku";
              repo = "calibre-web";
              rev = "6157f5027c979aa05f8d97a09f1388ceb3085ac5";
              hash = "sha256-1ljMsf8Puvq4ELUSi8Vl3T7EHcd7MO3zGgT4j5PYsT0=";
            };
          });
        })
        # TODO remove when https://github.com/NixOS/nixpkgs/pull/494483 is available in nixos-unstable
        (final: prev: {
          calibre = prev.callPackage ./pkgs/calibre/package.nix { };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      specialArgs = {
        inherit inputs;
        pkgs-unstable = pkgs;
      };
      home-manager-modules = [
        # inputs.plasma-manager.homeModules.plasma-manager
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
        dog = buildHomeFromNixos nixosConfigurations.dogdot.config.users.users.dog ./home.nix;
      };

      nixosConfigurations = {
        dogdot = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            nixos-hardware.nixosModules.common-cpu-intel
            inputs.authentik-nix.nixosModules.default
            ./configuration.nix
          ];
        };
      };
    };
}
