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
        rev = "ba1dca322f9a07bc2b7bec6a98f2c3c55c0bbd77";
        hash = "sha256-sebujw5VvBWMS+wXyjiGF81iyjPM/QQDnw5l7tDJCvk=";
      };
    };
  };
}
