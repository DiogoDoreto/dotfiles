{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.programs.t3code;
  localBackendEnabled = lib.boolToString cfg.desktop.localBackend.enable;
in
{
  options.dog.programs.t3code = {
    enable = lib.mkEnableOption "T3 Code CLI";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.t3code.override { providerPackages = [ ]; };
      defaultText = lib.literalExpression "pkgs.llm-agents.t3code.override { providerPackages = [ ]; }";
      description = "T3 Code package. By default, agents must be installed explicitly rather than bundled.";
    };

    desktop = {
      enable = lib.mkEnableOption "T3 Code desktop app";
      localBackend.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Allow the desktop to start its own backend. Disable when pairing with a separately managed backend, including one on the same machine.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ] ++ lib.optional cfg.desktop.enable cfg.package.desktop;

    # T3 also stores UI-managed preferences here; preserve them and keep this mutable.
    # Pairing credentials and environment connections are deliberately not managed.
    home.activation.t3codeDesktopSettings = lib.mkIf cfg.desktop.enable (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        settings_dir="$HOME/.t3/userdata"
        settings_file="$settings_dir/desktop-settings.json"
        if [ ! -e "$settings_file" ] || ! ${pkgs.jq}/bin/jq -e '.localEnvironmentEnabled == ${localBackendEnabled}' "$settings_file" >/dev/null; then
          run mkdir -p "$settings_dir"
          if [[ ! -v DRY_RUN ]]; then
            settings_tmp=$(mktemp "$settings_dir/.desktop-settings.XXXXXX")
            if ${pkgs.jq}/bin/jq '. + {localEnvironmentEnabled: ${localBackendEnabled}}' \
              <(if [ -e "$settings_file" ]; then ${pkgs.coreutils}/bin/cat "$settings_file"; else echo '{}'; fi) > "$settings_tmp"; then
              mv "$settings_tmp" "$settings_file"
            else
              rm -f "$settings_tmp"
              exit 1
            fi
          fi
        fi
      ''
    );
  };
}
