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

  tomlkit = pkgs.python3.withPackages (ps: [ ps.tomlkit ]);
  dotfilesProjectPath = builtins.unsafeDiscardStringContext (toString config.dog.dotfilesPath);

  # REMINDER: removing a setting from here does not remove it from the config file
  defaultSettings = {
    projects.${dotfilesProjectPath}.trust_level = "trusted";
  };

  defaultReadOnlyNixRules = ''
    prefix_rule(pattern=["nix", "eval"], decision="allow")
    prefix_rule(pattern=["nix", "flake", "metadata"], decision="allow")
    prefix_rule(pattern=["nix", "flake", "show"], decision="allow")
    prefix_rule(pattern=["nix", "path-info"], decision="allow")
    prefix_rule(pattern=["nix-store", "--query"], decision="allow")
    prefix_rule(pattern=["nix-store", "-q"], decision="allow")
  '';
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
    home.activation.codexConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      config_file="$HOME/.codex/config.toml"
      mkdir -p "$(dirname "$config_file")"
      [ -f "$config_file" ] || touch "$config_file"

      tmp=$(mktemp)
      ${tomlkit}/bin/python - "$config_file" "$tmp" ${lib.escapeShellArg (builtins.toJSON defaultSettings)} <<'PY'
      import json
      import sys
      import tomlkit

      source, target, defaults_json = sys.argv[1:]

      with open(source, "r", encoding="utf-8") as f:
          current = tomlkit.parse(f.read())

      defaults = json.loads(defaults_json)

      def merge(dst, src):
          for key, value in src.items():
              if isinstance(value, dict):
                  existing = dst.get(key)
                  if not isinstance(existing, dict):
                      existing = tomlkit.table()
                      dst[key] = existing
                  merge(existing, value)
              else:
                  dst[key] = value

      merge(current, defaults)

      with open(target, "w", encoding="utf-8") as f:
          f.write(tomlkit.dumps(current))
      PY
      mv "$tmp" "$config_file"
    '';

    home.file.".codex/rules/nix-read-only.rules".text = defaultReadOnlyNixRules;

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
