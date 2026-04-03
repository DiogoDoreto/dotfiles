{
  config,
  lib,
  dog-lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.claude-code;
  inherit (dog-lib) bubblewrapAi;
in
{
  options.dog.programs.claude-code = {
    enable = mkEnableOption "claude-code";

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
        package = pkgs.llm-agents.claude-code;
        extraWritablePaths = [
          "~/.claude/"
          "~/.claude.json"
        ]
        ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
        extraCommandFlags = [
          "--dangerously-skip-permissions"
        ];
      })
      (bubblewrapAi {
        package = pkgs.claude-agent-acp;
        extraWritablePaths = [
          "~/.claude/"
          "~/.claude.json"
        ]
        ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
      })
    ];
  };
}
