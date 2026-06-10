{
  config,
  lib,
  dog-lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.codex;
  inherit (dog-lib) bubblewrapAi;
in
{
  options.dog.programs.codex = {
    enable = mkEnableOption "codex";

    bubblewrap = {
      enable = mkEnableOption "Whether to wrap the commands in a bubblewrap sandbox.";

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
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        wrapIfNeeded = args: if cfg.bubblewrap.enable then bubblewrapAi args else args.package;
        extraWritablePaths = [
          "~/.codex/"
        ]
        ++ cfg.bubblewrap.extraWritablePaths;
      in
      [
        (wrapIfNeeded {
          inherit extraWritablePaths;
          package = pkgs.llm-agents.codex;
          extraReadOnlyPaths = cfg.bubblewrap.extraReadOnlyPaths;
        })
        (wrapIfNeeded {
          inherit extraWritablePaths;
          package = pkgs.llm-agents.codex-acp;
          extraReadOnlyPaths = cfg.bubblewrap.extraReadOnlyPaths;
        })
      ];
  };
}
