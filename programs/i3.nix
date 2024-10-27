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
          modifier = "Mod4"; # Windows key
          terminal = cfg.terminal;
          gaps.inner = 10;
          workspaceAutoBackAndForth = true;
          menu = strings.concatStringsSep " " [
            (lib.getExe pkgs.rofi)
            "-show combi"
            "-modes combi"
            "-combi-modes \"window#drun#Power:rofi-power-menu --choices=shutdown/reboot --confirm=logout\""
          ];
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
          bars = [{
            mode = "dock";
            hiddenState = "hide";
            position = "bottom";
            workspaceButtons = true;
            workspaceNumbers = true;
            statusCommand = "${pkgs.i3status}/bin/i3status";
            fonts = {
              names = [ "VictorMono Nerd Font Propo" ];
              size = 10.0;
            };
            trayOutput = "primary";
            colors = {
              background = "#000000";
              statusline = "#ffffff";
              separator = "#666666";
              focusedWorkspace = {
                border = "#4c7899";
                background = "#285577";
                text = "#ffffff";
              };
              activeWorkspace = {
                border = "#333333";
                background = "#5f676a";
                text = "#ffffff";
              };
              inactiveWorkspace = {
                border = "#333333";
                background = "#222222";
                text = "#888888";
              };
              urgentWorkspace = {
                border = "#2f343a";
                background = "#900000";
                text = "#ffffff";
              };
              bindingMode = {
                border = "#2f343a";
                background = "#900000";
                text = "#ffffff";
              };
            };
          }];
        };
      };
    };

    home.file = {
      ".config/i3status/config".text = ''
        general {
          colors = true
          interval = 5
        }

        order += "wireless _first_"
        order += "ethernet _first_"
        order += "disk /"
        order += "load"
        order += "memory"
        order += "battery all"
        order += "tztime local"

        wireless _first_ {
          format_up = " (%quality at %essid) %ip"
          format_down = "󰖪"
        }

        ethernet _first_ {
          format_up = " %ip (%speed)"
          format_down = "󰲜"
        }

        battery all {
          format = "%status %percentage %remaining"
          format_down = "󱉞"
        }

        disk "/" {
          format = " %avail"
        }

        load {
          format = " %1min"
        }

        memory {
          format = " %used | %available"
          threshold_degraded = "1G"
          format_degraded = "MEMORY < %available"
        }

        tztime local {
          format = "󰸘 %Y-%m-%d %H:%M:%S"
        }
      '';
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
