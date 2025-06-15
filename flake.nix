{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL/310f8e49a149e4c9ea52f1adf70cdc768ec53f8a";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-comfyui = {
      url = "github:dyscorv/nix-comfyui";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };

    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    whisper-to-text = {
      # https://github.com/DiogoDoreto/whisper-to-text
      url = "git+file:///home/dog/projects/whisper-to-text";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, home-manager, nixos-hardware, ... }@inputs:
    let
      system = "x86_64-linux";
      overlays = [
        inputs.nixgl.overlay
        inputs.nur.overlays.default
        inputs.nix-comfyui.overlays.default
      ];
      pkgs-config = {
        inherit system overlays;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      pkgs = import nixpkgs pkgs-config;
      pkgs-unstable = import nixpkgs-unstable pkgs-config;
      specialArgs = { inherit inputs pkgs-unstable; };
      extraSpecialArgs = specialArgs;
    in {
      homeConfigurations = {
        "dog@dogdot" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [ ./home/home.nix ];
        };
        "dog@chungus" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [
            { home = { username = "dog"; homeDirectory = "/home/dog"; }; }
            ./hosts/chungus/home.nix
          ];
        };
        "dog@mini" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [
            { home = { username = "dog"; homeDirectory = "/home/dog"; }; }
            ./hosts/mini/home.nix
          ];
        };
        work = home-manager.lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [ ./home/work.nix ];
        };
      };

      nixosConfigurations = {
        chungus = nixpkgs.lib.nixosSystem rec {
          inherit system specialArgs;
          modules = [
            ./hosts/chungus/configuration.nix
            ./nix-config.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = specialArgs;
              nixpkgs = { inherit overlays; };
            }
          ];
        };
        mini = nixpkgs.lib.nixosSystem rec {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.common-cpu-intel
            ./hosts/mini/configuration.nix
            ./nix-config.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = specialArgs;
              nixpkgs = { inherit overlays; };
            }
          ];
        };
        inspiron7520 = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/inspiron7520/hardware.nix
            ./hosts/inspiron7520/configuration.nix
            ./nix-config.nix
            home-manager.nixosModules.home-manager
            nixos-hardware.nixosModules.common-cpu-intel
            nixos-hardware.nixosModules.common-gpu-amd
            nixos-hardware.nixosModules.common-pc-laptop
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            {
              nixpkgs.overlays = [
                inputs.nur.overlays.default
              ];
            }
          ];
          specialArgs = { inherit inputs; };
        };
      };
    };
}
