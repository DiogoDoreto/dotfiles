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
        "-combi-modes \"window#drun#Power:${getExe pkgs.rofi-power-menu} --confirm=hibernate\""
      ];
    };
  };

  config = mkIf cfg.enable {
    xsession.windowManager.i3.config.keybindings = lib.mkOptionDefault {
      "Mod4+comma" = "exec rofi -plugin-path ${pkgs.rofi-calc}/lib/rofi -show calc -modi calc -no-show-match -no-sort | xclip -sel clip";
      "Mod4+period" = "exec ${getExe pkgs.rofimoji}";
      "Mod4+Mod1+period" = "exec ${getExe pkgs.rofimoji} -f nerd_font";
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
  height: 530px;
  border: 1px;
  border-color: ${cfg.colors.active};
}

mainbox {
  children: [ inputbar, message, listview ];
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

message {
  border: 1px 0;
  border-color: ${cfg.colors.active};
  padding: 15px 20px;
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
  padding: 8px 20px 8px;
  border: 0 0 2px;
  border-color: transparent;
}

element-icon {
  size: 18px;
  margin: 0 20px 0 0;
}

element selected {
  background-color: ${cfg.colors.active};
  border-color: ${cfg.colors.background}66;
}
      '';
      in toString themeFile;
    };
  };
}
