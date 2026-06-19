{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.forgejo-cli;

  # PR #473: add env attribute to clap derives so FJ_HOST and FJ_STYLE env vars are recognised
  forgejo-cli = pkgs.forgejo-cli.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (pkgs.fetchpatch {
        url = "https://codeberg.org/forgejo-contrib/forgejo-cli/pulls/473.patch";
        hash = "sha256-sD3hJsFGGVybvyEilt8SZb5AuErOOW5ZL+oY8jbbJBU=";
      })
    ];
  });
in
{
  options.dog.programs.forgejo-cli = {
    enable = mkEnableOption "forgejo-cli";

    host = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "codeberg.org";
      description = "The Forgejo host to target. Sets the FJ_HOST environment variable when non-null.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ forgejo-cli ];

    home.sessionVariables = {
      FJ_HOST = mkIf (cfg.host != null) cfg.host;
    };
  };
}
