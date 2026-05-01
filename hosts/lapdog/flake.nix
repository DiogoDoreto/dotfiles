{
  description = "Lapdog host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
    my-niri = {
      url = "../../modules/flakes/niri";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-kwtype = {
      url = "../../modules/flakes/kwtype";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-ipu7 = {
      url = "../../modules/flakes/ipu7";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-ghostel = {
      url = "../../flakes/ghostel";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-claude-agent-acp = {
      url = "../../flakes/claude-agent-acp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-pi = {
      url = "../../flakes/pi";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llm-agents.url = "github:numtide/llm-agents.nix";
    handy = {
      url = "github:cjpais/Handy";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    forgejo-cli.url = "git+https://codeberg.org/forgejo-contrib/forgejo-cli";
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
      ...
    }@inputs:
    let
      system = "x86_64-linux";

      overlays = [
        inputs.llm-agents.overlays.default
        inputs.nur.overlays.default

        inputs.my-claude-agent-acp.overlays.${system}.default
        inputs.my-ghostel.overlays.${system}.default
        inputs.my-kwtype.overlays.default
        inputs.my-pi.overlays.${system}.default

        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
          inherit (inputs.forgejo-cli.packages.${system}) forgejo-cli;
          handy = inputs.handy.packages.${system}.default;
        })
      ]
      ++ inputs.my-ipu7.outputs.overlays
      ++ inputs.my-niri.outputs.overlays;

      specialArgs = {
        inherit inputs self;
        pkgs-unstable = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
          config.allowUnfreePredicate = _: true;
        };
      };

      nixos-modules = [
        (import ../../nix-config.nix inputs)
        home-manager.nixosModules.home-manager
        {
          nixpkgs = { inherit overlays; };
          home-manager.extraSpecialArgs = specialArgs;
          home-manager.sharedModules = [
            ../../modules/home-manager
          ];
        }
      ];
    in
    {
      nixosConfigurations = {
        lapdog = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules =
            nixos-modules
            ++ inputs.my-ipu7.outputs.nixosModules
            ++ inputs.my-niri.outputs.nixosModules
            ++ [
              inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga
              inputs.musnix.nixosModules.musnix
              inputs.microvm.nixosModules.host
              {
                home-manager.sharedModules = inputs.my-niri.outputs.homeModules ++ [
                  inputs.handy.homeManagerModules.default
                ];
              }
              ./configuration.nix
            ];
        };

        # Coding-agent MicroVM guest OS definition.
        # The lapdog host's microvm.nixosModules.host picks this up via
        # microvm.vms."lapdog-agent".flake = self in configuration.nix.
        # Manage with: systemctl start/stop microvm@lapdog-agent
        lapdog-agent = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            inputs.microvm.nixosModules.microvm
            ./microvm-guest.nix
          ];
        };
      };
    };
}
