{
  config,
  lib,
  dog-lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.pi;
  piPackage =
    package:
    if cfg.sandbox.enable then
      dog-lib.bubblewrapAi {
        inherit package;
        extraWritablePaths = [ "~/.pi/" ] ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
      }
    else
      package;
in
{
  options.dog.programs.pi = {
    enable = mkEnableOption "pi";

    sandbox.enable = mkOption {
      type = types.bool;
      default = true;
      description = "Run Pi and pi-acp inside bubblewrap. Disable when already isolated in an Agent VM.";
    };

    extraWritablePaths = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra writable paths to bind mount into the bubblewrap sandbox (when enabled).";
    };

    extraReadOnlyPaths = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra read-only paths to bind mount into the bubblewrap sandbox (when enabled).";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (piPackage pkgs.llm-agents.pi)
      (piPackage pkgs.llm-agents.pi-acp)
    ];

    home.file.".pi/agent/extensions/pi-web-access".source = pkgs.pi-web-access;
  };
}
