{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.i3;
in
{
  options.dog.programs.i3 = {
    enable = mkEnableOption "i3";

    terminal = mkOption {
      type = types.str;
      default = "wezterm";
      description = "Default terminal program";
    };
  };

  config = mkIf cfg.enable {
    xsession = {
      enable = true;

      windowManager.i3 = {
        enable = true;
        config = {
          menu = strings.concatStringsSep " " [
            (lib.getExe pkgs.rofi)
            "-show combi"
            "-modes combi"
            "-combi-modes \"window#drun#Power:rofi-power-menu --choices=shutdown/reboot --confirm=logout\""
          ];
          modifier = "Mod4"; # Windows key
          terminal = cfg.terminal;
          gaps.inner = 10;
          keybindings = let
            modifier = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault {
            # Mod1 == Alt key
            "Ctrl+Shift+Mod1+e" = "exec emacsclient -nc";
            "Ctrl+Shift+Mod1+f" = "exec firefox";
            "Ctrl+Shift+Mod1+k" = "exec keepassxc";
            "Ctrl+Shift+Mod1+m" = "exec spotify";
            "Ctrl+Shift+Mod1+t" = "exec ${cfg.terminal}";
            "${modifier}+period" = "exec rofimoji";
          };
        };
      };
    };

    home.packages = with pkgs; [
      rofimoji
      rofi-power-menu
    ];

    programs.rofi = {
      enable = true;
      font = "VictorMono Nerd Font 12";
      theme = "${inputs.rofi-material-ocean}/material-ocean.rasi";
      terminal = cfg.terminal;
      extraConfig = {
        show-icons = true;
        combi-hide-mode-prefix = true;
      };
    };
  };
}
