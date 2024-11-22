{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.emacs;
in
{
  options.dog.programs.emacs = {
    enable = mkEnableOption "emacs + doom";

    extraPackages = mkOption {
      type = types.lines;
      default = "";
    };
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
      "doom/packages.el".text = (readFile ../.config/doom/packages.el)
        + "\n\n" + cfg.extraPackages;
      "doom/dd".source = ../.config/doom/dd;

      emacs.source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "be4fb85dd93810e495d5f4a793d620f82508cb7e";
        hash = "sha256-13DX27VcSe9lLz7go2tFdNLoroNzlcboD5I2mHfL0Ms=";
      };
    };
  };
}
