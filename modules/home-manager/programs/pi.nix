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
  inherit (dog-lib) bubblewrapAi;
in
{
  options.dog.programs.pi = {
    enable = mkEnableOption "pi";

    extraWritablePaths = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra writable paths to bind mount into the bubblewrap sandbox.";
    };

    extraReadOnlyPaths = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Extra read-only paths to bind mount into the bubblewrap sandbox.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (bubblewrapAi {
        package = pkgs.llm-agents.pi;
        extraWritablePaths = [
          "~/.pi/"
        ]
        ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
      })
      (bubblewrapAi {
        package = pkgs.pi-acp;
        extraWritablePaths = [
          "~/.pi/"
        ]
        ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
      })
    ];
  };
}
