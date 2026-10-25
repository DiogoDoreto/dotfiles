{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.forgejo-cli;

  forgejo-cli =
    if cfg.host != null then
      pkgs.symlinkJoin {
        name = "forgejo-cli";
        paths = [ pkgs.forgejo-cli ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/fj" \
            --run ${escapeShellArg ''
              # Callers such as t3code select a host explicitly.
              explicit_host=false
              for arg in "$@"; do
                case "$arg" in
                  --) break ;;
                  -H| -H?*| --host| --host=*) explicit_host=true; break ;;
                esac
              done
              if ! "$explicit_host"; then
                set -- --host ${escapeShellArg cfg.host} "$@"
              fi
            ''}
        '';
      }
    else
      pkgs.forgejo-cli;
in
{
  options.dog.programs.forgejo-cli = {
    enable = mkEnableOption "forgejo-cli";

    host = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "codeberg.org";
      description = "The default Forgejo host to target when no explicit `--host` or `-H` flag is supplied.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ forgejo-cli ];
  };
}
