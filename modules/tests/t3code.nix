# Evaluation-only fixtures: no host builds or activation.
{ flake }:
let
  inherit (flake) inputs;
  pkgs = flake.nixosConfigurations.chungus.pkgs;
  lib = inputs.nixpkgs.lib;
  homeConfig =
    options:
    (inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        ../home-manager/programs/t3code.nix
        {
          home.username = "tester";
          home.homeDirectory = "/home/tester";
          home.stateVersion = "25.05";
          dog.programs.t3code = options;
        }
      ];
    }).config;
  systemConfig =
    options:
    (lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ../nixos/services/t3code.nix
        {
          nixpkgs.pkgs = pkgs;
          system.stateVersion = "25.05";
          users.users.tester = {
            isNormalUser = true;
            home = "/home/tester";
          };
          dog.services.t3code = options;
        }
      ];
    }).config;
  packages = c: builtins.filter (p: (p.pname or "") == "t3code") c.home.packages;
  disabledHome = homeConfig { };
  cli = homeConfig { enable = true; };
  desktop = homeConfig {
    enable = true;
    desktop.enable = true;
  };
  pairedDesktop = homeConfig {
    enable = true;
    desktop.enable = true;
    desktop.localBackend.enable = false;
  };
  disabledSystem = systemConfig { };
  backend = systemConfig {
    enable = true;
    user = "tester";
  };
  customBackend = systemConfig {
    enable = true;
    user = "tester";
    host = "192.168.0.3";
    port = 4321;
    workingDirectory = "/home/tester/projects";
    stateDirectory = "/home/tester/custom-state";
  };
  invalidUser = systemConfig {
    enable = true;
    user = "missing";
  };
  invalidPaths = systemConfig {
    enable = true;
    user = "tester";
    stateDirectory = "relative";
  };
  hasFailure = message: c: builtins.any (a: a.message == message && !a.assertion) c.assertions;
in
assert packages disabledHome == [ ];
assert !(disabledHome.home.activation ? t3codeDesktopSettings);
assert builtins.length (packages cli) == 1;
assert !(cli.home.activation ? t3codeDesktopSettings);
assert builtins.length (packages desktop) == 2;
assert builtins.all (p: p.providerPackages == [ ]) (packages desktop);
assert !(disabledSystem.systemd.services ? t3code);
assert backend.dog.services.t3code.host == "127.0.0.1";
assert backend.networking.firewall.allowedTCPPorts == [ ];
assert
  backend.networking.firewall.extraCommands == disabledSystem.networking.firewall.extraCommands;
assert backend.systemd.services.t3code.wantedBy == [ "multi-user.target" ];
assert backend.systemd.services.t3code.serviceConfig.User == "tester";
assert backend.systemd.services.t3code.serviceConfig.Restart == "on-failure";
assert backend.systemd.services.t3code.serviceConfig.UMask == "0077";
assert backend.systemd.services.t3code.environment.HOME == "/home/tester";
assert backend.systemd.services.t3code.environment.T3CODE_HOME == "/home/tester/.t3";
assert backend.systemd.services.t3code.serviceConfig.WorkingDirectory == "/home/tester";
assert customBackend.systemd.services.t3code.environment.T3CODE_HOME == "/home/tester/custom-state";
assert
  customBackend.systemd.services.t3code.serviceConfig.WorkingDirectory == "/home/tester/projects";
assert lib.hasInfix "--host 192.168.0.3 --port 4321"
  customBackend.systemd.services.t3code.serviceConfig.ExecStart;
assert hasFailure "dog.services.t3code.user must name an existing users.users entry." invalidUser;
assert hasFailure "T3 Code workingDirectory and stateDirectory must be absolute paths."
  invalidPaths;
{
  checks = "passed";
  activation = {
    enabled = desktop.home.activation.t3codeDesktopSettings.data;
    disabled = pairedDesktop.home.activation.t3codeDesktopSettings.data;
  };
}
