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
    llama-cpp = {
      url = "github:ggml-org/llama.cpp/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doomemacs = {
      url = "git+https://github.com/doomemacs/doomemacs?submodules=1";
      flake = false;
    };
    my-kwtype = {
      url = "../../modules/flakes/kwtype";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-ipu7 = {
      url = "../../modules/flakes/ipu7";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llm-agents.url = "github:numtide/llm-agents.nix";
    linux-systems.url = "github:nix-systems/x86_64-linux";
    handy = {
      url = "github:cjpais/Handy";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.bun2nix.inputs.systems.follows = "linux-systems";
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
        inputs.nur.overlays.default

        inputs.my-kwtype.overlays.default

        (final: prev: {
          inherit (inputs.home-manager.packages.${system}) home-manager;
          inherit (inputs.forgejo-cli.packages.${system}) forgejo-cli;
          handy = inputs.handy.packages.${system}.default;
          llama-cpp = inputs.llama-cpp.packages.${system}.vulkan;
          llm-agents = inputs.llm-agents.packages.${system};
        })
      ]
      ++ builtins.attrValues inputs.my-ipu7.outputs.overlays;

      opencodeAgentVm = {
        workingDirectory = "/home/agent/projects";
        dnsForwardZones = [
          {
            domain = "local.doreto.com.br";
            server = "192.168.0.2";
          }
          {
            domain = "home";
            server = "192.168.0.2";
          }
        ];
        shares = [
          {
            tag = "projects";
            source = "/home/dog/projects";
            mountPoint = "/home/agent/projects";
            readOnly = false;
          }
        ];
      };

      specialArgs = {
        inherit inputs self opencodeAgentVm;
        pkgs-unstable = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
          config.allowUnfreePredicate = _: true;
        };
      };

      nixos-modules = [
        (import ../../nix-config.nix inputs)
        ../../modules/nixos
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
            ++ builtins.attrValues inputs.my-ipu7.outputs.nixosModules
            ++ [
              inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-yoga
              inputs.musnix.nixosModules.musnix
              inputs.microvm.nixosModules.host
              {
                home-manager.sharedModules = [
                  inputs.handy.homeManagerModules.default
                ];
              }
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
