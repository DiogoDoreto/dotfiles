{
  description = "Mini host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    authentik-nix.url = "github:nix-community/authentik-nix";
    llm-agents.url = "github:numtide/llm-agents.nix";
    paseo-src = {
      url = "github:getpaseo/paseo/v0.10.2";
      flake = false;
    };
    git-pages = {
      url = "git+https://codeberg.org/git-pages/git-pages";
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
      url = "git+https://github.com/doomemacs/doomemacs?submodules=1";
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
        (final: prev: {
          # https://github.com/NixOS/nixpkgs/pull/540681
          calibre-web = prev.calibre-web.overridePythonAttrs (old: {
            pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [
              "certifi"
              "chardet"
            ];
          });
          pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
            (python-final: python-prev: {
              pip-chill = python-prev.pip-chill.overridePythonAttrs (old: {
                doCheck = false;
                pythonImportsCheck = [ ];
              });
            })
          ];
          inherit (inputs.home-manager.packages.${system}) home-manager;
          llm-agents = inputs.llm-agents.packages.${system};
          my = (prev.my or { }) // {
            denon = final.callPackage ./packages/denon.nix { };
          };
          orgnotes = inputs.nextcloud-org-notes.packages.${system}.default;
          # https://github.com/getpaseo/paseo/pull/3853
          # Resolve node-pty from the server workspace when tracing the daemon
          # closure, so the native pty.node prebuild is packaged whether npm
          # hoists node-pty or installs it workspace-locally. Without it,
          # terminal creation fails with "Terminal worker is not running".
          paseo =
            (final.callPackage (inputs.paseo-src.outPath + "/nix/package.nix") {
              npmDepsHash = "sha256-tT7qrQpJSxXTJMc9KinfnDQoeTdvLt7NWanYANKunqg=";
            }).overrideAttrs
              (old: {
                pname = "${old.pname}-pr-3853";
                patches = (old.patches or [ ]) ++ [ ./patches/paseo-pr-3853.patch ];
              });
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
        paseo.enable = true;
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
            (nixos-hardware.outPath + "/common/cpu/intel/alder-lake")
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
                guest.tailscale.enable = true;
              };

              nix.gc = {
                automatic = true;
                dates = "*-*-* 03:00:00 Europe/Madrid";
                persistent = false;
              };

              systemd.services.nix-gc = {
                unitConfig.RequiresMountsFor = [ "/nix/.rw-store" ];
                serviceConfig.ExecCondition = pkgs.writeShellScript "opencode-agent-vm-gc-low-space" ''
                  set -euo pipefail
                  stats="$(${pkgs.coreutils}/bin/stat -f -c '%a %S' /nix/.rw-store)"
                  read -r available_blocks block_size <<< "$stats"
                  (( available_blocks * block_size < 15 * 1024 * 1024 * 1024 ))
                '';
              };
            }
          ];
        };
      };
    };
}
