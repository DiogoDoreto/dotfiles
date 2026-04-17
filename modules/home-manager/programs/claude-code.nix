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

  defaultSettings = {
    "\$schema" = "https://json.schemastore.org/claude-code-settings.json";
    permissions.defaultMode = "bypassPermissions";
    skipDangerousModePermissionPrompt = true;
    plansDirectory = "./.private/plans";
    attribution = {
      commit = "";
      pr = "";
    };
  };
in
{
  options.dog.programs.claude-code = {
    enable = mkEnableOption "claude-code";

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
    home.activation.claudeCodeSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      settings="$HOME/.claude/settings.json"
      mkdir -p "$(dirname "$settings")"
      [ -f "$settings" ] || echo '{}' > "$settings"
      tmp=$(mktemp)
      ${pkgs.jq}/bin/jq --argjson d ${lib.escapeShellArg (builtins.toJSON defaultSettings)} '. * $d' "$settings" > "$tmp" && mv "$tmp" "$settings"
    '';

    home.packages =
      let
        wrapIfNeeded = args: if cfg.bubblewrap.enable then bubblewrapAi args else args.package;
        extraWritablePaths = [
          "~/.claude/"
          "~/.claude.json"
        ]
        ++ cfg.bubblewrap.extraWritablePaths;
      in
      [
        (wrapIfNeeded {
          inherit extraWritablePaths;
          package = pkgs.llm-agents.claude-code;
          extraReadOnlyPaths = cfg.bubblewrap.extraReadOnlyPaths;
          extraCommandFlags = [
            "--dangerously-skip-permissions"
          ];
        })
        (wrapIfNeeded {
          inherit extraWritablePaths;
          package = pkgs.claude-agent-acp;
          extraReadOnlyPaths = cfg.bubblewrap.extraReadOnlyPaths;
        })
      ];
  };
}
