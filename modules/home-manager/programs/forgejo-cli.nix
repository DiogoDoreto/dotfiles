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
            --add-flags "--host ${escapeShellArg cfg.host}"
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
      description = "The Forgejo host to target. Hardcodes the `--host` flag on every `fj` invocation when non-null.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ forgejo-cli ];
  };
}
