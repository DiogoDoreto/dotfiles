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
    nextcloud-org-notes = {
      url = "path:/home/dog/projects/nextcloud-org-notes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:astro/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
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
          orgnotes = inputs.nextcloud-org-notes.packages.${system}.default;
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      opencodeAgentVm = {
        autostart = true;
        workingDirectory = "/home/agent";
        dnsmasqBindBridgeInterface = false;
        dnsmasqBindInterfaces = false;
        dnsUpstreams = [ ];
        caddy.enable = true;
      };
      specialArgs = {
        inherit inputs self opencodeAgentVm;
        pkgs-unstable = pkgs;
      };
      home-manager-modules = [
        ../../modules/home-manager
      ];
      nixos-modules = [
        (import ../../nix-config.nix inputs)
        ../../modules/nixos
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
            inputs.microvm.nixosModules.host
            ./configuration.nix
          ];
        };

        opencode-agent-vm = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            inputs.microvm.nixosModules.microvm
            {
              dog.services.opencode-agent-vm = opencodeAgentVm // {
                guest.enable = true;
              };
            }
          ];
        };
      };
    };
}
