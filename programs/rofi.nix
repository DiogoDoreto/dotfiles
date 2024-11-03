{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.rofi;
in
{
  options.dog.programs.rofi = {
    enable = mkEnableOption "Rofi";

    colors = mkOption {
      type = types.submodule {
        options = {
          background = mkOption { type = types.str; };
          text = mkOption { type = types.str; };
          active = mkOption { type = types.str; };
        };
      };
    };

    command = mkOption {
      type = types.str;
      default = strings.concatStringsSep " " [
        (getExe pkgs.rofi)
        "-show combi"
        "-modes combi"
        "-combi-modes \"window#drun#Power:${getExe pkgs.rofi-power-menu} --choices=shutdown/reboot --confirm=logout\""
      ];
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      rofimoji
      rofi-power-menu
    ];

    xsession.windowManager.i3.config.keybindings = lib.mkOptionDefault {
      "Mod4+period" = "exec rofimoji";
      "Mod4+Mod1+period" = "exec rofimoji -f nerd_font";
    };

    programs.rofi = {
      enable = true;
      font = "VictorMono Nerd Font 12";
      terminal = "wezterm";
      extraConfig = {
        show-icons = true;
        combi-hide-mode-prefix = true;
      };
      theme = let
        themeFile = pkgs.writeText "theme.rasi" ''
* {
  background-color: transparent;
  text-color: ${cfg.colors.text};
}

window {
  transparency: "real";
  background-image: linear-gradient(${cfg.colors.background}, ${cfg.colors.background}99);
  width: 900px;
  border: 1px;
  border-color: ${cfg.colors.active};
}

mainbox {
  children: [ inputbar, listview ];
  spacing: 0;
  padding: 0;
}

inputbar {
  children: [ textbox-prompt-colon, entry ];
  expand: false;
  border: 0;
  margin: 0;
  padding: 15px 20px;
  position: center;
}

prompt {
  enabled: true;
}

textbox-prompt-colon {
  expand: false;
  str: "";
  margin: 0 20px 0 0;
}

entry {
  expand: true;
  horizontal-align: 0;
  blink: true;
}

case-indicator {
  spacing: 0;
}

listview {
  columns: 1;
  spacing: 5px;
  cycle: true;
  dynamic: true;
  layout: vertical;
}

element {
  orientation: horizontal;
  padding: 10px 20px;
}

element-icon {
  size: 18px;
  margin: 0 20px 0 0;
}

element selected {
  background-color: ${cfg.colors.active};
}
      '';
      in toString themeFile;
    };
  };
}
