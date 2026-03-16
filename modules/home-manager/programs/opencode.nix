{
  config,
  lib,
  dog-lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.opencode;
  inherit (dog-lib) dotfilesSymlink bubblewrapAi;
in
{
  options.dog.programs.opencode = {
    enable = mkEnableOption "opencode";

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

    configSource = mkOption {
      type = types.str;
      default = ".config/opencode";
      description = "Dotfiles-relative path used as the source for the opencode XDG config directory.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (bubblewrapAi {
        package = pkgs.llm-agents.opencode;
        extraWritablePaths = [
          "~/.config/opencode/"
          "~/.local/share/opencode/"
        ]
        ++ cfg.extraWritablePaths;
        extraReadOnlyPaths = cfg.extraReadOnlyPaths;
      })
    ];

    xdg.configFile = {
      "opencode".source = dotfilesSymlink cfg.configSource;
    };

    # ensure folder exists, otherwise bwrap fails to start
    systemd.user.tmpfiles.rules = [
      "d %h/.local/share/opencode 0755 - - - -"
    ];
  };
}
