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

    home.packages = with pkgs; [
      sqlite # for org-roam
    ];

    home.sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
    ];

    home.sessionVariables = {
      DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
      DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };

    xdg.configFile = {
      "doom/init.el".source = ../.config/doom/init.el;
      "doom/config.el".source = ../.config/doom/config.el;
      "doom/packages.el".source = ../.config/doom/packages.el;

      emacs.source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "6a8c09f01288f1ed00a7cc2b7f5887e8f2b4be77";
        hash = "sha256-VEwF0xjr+lBYuNpA3U3mFJPrQLk/7PF0/G/jgbIMIoE=";
#        leaveDotGit = true;
      };
    };
  };
}
