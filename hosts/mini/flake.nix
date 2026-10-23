{
  description = "Mini host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    authentik-nix.url = "github:nix-community/authentik-nix";
    llm-agents.url = "github:numtide/llm-agents.nix";
    linux-systems.url = "github:nix-systems/x86_64-linux";
    handy = {
      url = "github:cjpais/Handy/9e57a0d7929fcc6417ea0d6e54c1b57f62635edd";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.bun2nix.inputs.systems.follows = "linux-systems";
    };
    my-kwtype = {
      url = "../../modules/flakes/kwtype";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    paseo-src = {
      url = "github:getpaseo/paseo/v0.11.4";
      flake = false;
    };
    git-pages = {
      url = "../../modules/flakes/git-pages";
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
      url = "github:DiogoDoreto/nextcloud-org-notes";
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
        inputs.my-kwtype.overlays.default
        (final: prev: {
          calibre = prev.calibre.override { speechSupport = false; };
          # https://github.com/NixOS/nixpkgs/pull/540681
          calibre-web = prev.calibre-web.overridePythonAttrs (old: {
            pythonRelaxDeps = (old.pythonRelaxDeps or [ ]) ++ [
              "certifi"
              "chardet"
            ];
          });
          pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
            (python-final: python-prev: {
              pyopenjtalk-plus = python-prev.pyopenjtalk-plus.overridePythonAttrs (old: {
                # Fixed upstream: https://github.com/NixOS/nixpkgs/pull/576063
                # Remove once the pinned nixpkgs includes that fix.
                # Cython can reject non-str inputs before the explicit validation,
                # using "Expected str" rather than "must be str". Keep the tests.
                postPatch = (old.postPatch or "") + ''
                  substituteInPlace tests/test_frontend.py \
                    --replace-fail 'match="must be str"' 'match="must be str|Expected str"'
                '';
              });
              pip-chill = python-prev.pip-chill.overridePythonAttrs (old: {
                doCheck = false;
                pythonImportsCheck = [ ];
              });
            })
          ];
          inherit (inputs.home-manager.packages.${system}) home-manager;
          handy = inputs.handy.packages.${system}.default;
          llm-agents = inputs.llm-agents.packages.${system};
          my = (prev.my or { }) // {
            denon = final.callPackage ./packages/denon.nix { };
          };
          orgnotes = inputs.nextcloud-org-notes.packages.${system}.default;
          paseo = final.callPackage (inputs.paseo-src.outPath + "/nix/package.nix") {
            npmDepsHash = "sha256-nBFW6cm/4LO3q9BAsZSjBoNQN1i1PshCfcKbup0G/Lg=";
          };
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
        inherit inputs self;
        pkgs-unstable = pkgs;
      };
      home-manager-modules = [
        ../../modules/home-manager
        inputs.handy.homeManagerModules.default
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
            ./vms/agent-vm/host.nix
            ./configuration.nix
          ];
        };

        agent-vm = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = nixos-modules ++ [
            ./vms/agent-vm/guest.nix
          ];
        };
      };
    };
}
