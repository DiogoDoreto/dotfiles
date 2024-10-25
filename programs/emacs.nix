{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.emacs;
in
{
  options.dog.programs.emacs = {
    enable = mkEnableOption "emacs + doom";
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;

    services.emacs = {
      enable = true;
      defaultEditor = true;
    };

    home.sessionVariables = {
      DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
      DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };

    home.file = {
      ".config/emacs".source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "c8a5e6ec1ca85a35f94d6c820c2fd8888373c2ae";
        hash = "sha256-iwpBfHuJUd5jJjSGSXqlU9V0XKRNTeh6PvUq8riDnCE=";
      };
      ".config/doom".source = ../.config/doom;
    };
  };
}
