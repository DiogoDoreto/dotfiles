{
  description = "My Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nixos-wsl.url = "github:nix-community/NixOS-WSL/main";

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:nix-community/nixGL/310f8e49a149e4c9ea52f1adf70cdc768ec53f8a";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ghostty = {
      url = "github:ghostty-org/ghostty";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    nix-comfyui = {
      url = "github:dyscorv/nix-comfyui";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nixgl, nur, nixos-hardware, nixos-wsl, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ nixgl.overlay nur.overlays.default ];
      };
    in {
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

      nixosConfigurations = {
        chungus = nixpkgs.lib.nixosSystem rec {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/chungus/configuration.nix
            ./nix-config.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.extraSpecialArgs = specialArgs;
              nixpkgs.overlays = [
                nur.overlays.default
                inputs.nix-comfyui.overlays.default
              ];
            }
          ];
        };
        wsl = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            nixos-wsl.nixosModules.default
            ./hosts/wsl/configuration.nix
            ./nix-config.nix
            home-manager.nixosModules.home-manager
            # {
            #   nixpkgs.overlays = [
            #     nur.overlays.default
            #   ];
            # }
          ];
          specialArgs = { inherit inputs; };
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
                nur.overlays.default
              ];
            }
          ];
          specialArgs = { inherit inputs; };
        };
      };
    };
}
