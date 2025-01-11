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
    programs.emacs = {
      enable = true;
      package = pkgs.emacs30;
    };

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
      "doom/config-javascript.el".source = ../.config/doom/config-javascript.el;
      "doom/config.el" = {
        source = ../.config/doom/config.el;
        # try make it work later
        # onChange = "~/.config/emacs/bin/doom sync";
      };
      "doom/packages.el".text = (readFile ../.config/doom/packages.el)
        + "\n\n" + cfg.extraPackages;
      "doom/dd".source = ../.config/doom/dd;

      emacs.source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "1a8636056051ad52c8df33adc898699451c425a7";
        hash = "sha256-hyc8OiRiJMfDUxvx19mnoA+ZwSrva4wwSP6a1W4JP/I=";
      };
    };
  };
}
