{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.i3;
  colors = import ../utils/colors-beach.nix;
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
    home.packages = with pkgs; [
      feh
    ];

    xsession = {
      enable = true;

      windowManager.i3 = {
        enable = true;
        config = {
          modifier = "Mod4"; # Windows key
          terminal = cfg.terminal;
          gaps.inner = 10;
          gaps.bottom = 30;
          workspaceAutoBackAndForth = true;
          defaultWorkspace = "workspace number 1";
          menu = config.dog.programs.rofi.command;
          keybindings = let
            playerctl = getExe pkgs.playerctl;
            pactl = "${pkgs.pulseaudio}/bin/pactl";
          in mkOptionDefault {
            # Mod1 == Alt key
            "Ctrl+Shift+Mod1+e" = "exec emacsclient -nc";
            "Ctrl+Shift+Mod1+f" = "exec firefox";
            "Ctrl+Shift+Mod1+k" = "exec keepassxc";
            "Ctrl+Shift+Mod1+m" = "exec spotify";
            "XF86AudioPlay" = "exec ${playerctl} play-pause";
            "XF86AudioNext" = "exec ${playerctl} next";
            "XF86AudioPrev" = "exec ${playerctl} previous";
            "XF86AudioRaiseVolume" = "exec --no-startup-id ${pactl} set-sink-volume 0 +5%";
            "XF86AudioLowerVolume" = "exec --no-startup-id ${pactl} set-sink-volume 0 -5%";
            "XF86AudioMute" = "exec --no-startup-id ${pactl} set-sink-mute 0 toggle";
          };
          bars = [];
          startup = [
            {
              command = "${getExe pkgs.feh} --bg-fill ${../wallpapers/wallhaven-x1xlmv.jpg} --geometry=+0-300";
              always = true;
            }
          ];
          colors = {
            focused = {
              background = colors.Cerulean;
              border = colors.Cerulean;
              text = "#ffffff";
              indicator = colors.Cerulean;
              childBorder = colors.Cerulean;
            };
            focusedInactive = {
              background = colors.Caribbean-Current;
              border = colors.Caribbean-Current;
              text = "#cccccc";
              indicator = colors.Caribbean-Current;
              childBorder = colors.Caribbean-Current;
            };
            unfocused = {
              background = colors.Charcoal;
              border = colors.Charcoal;
              text = "#cccccc";
              indicator = colors.Charcoal;
              childBorder = colors.Charcoal;
            };
          };
        };
      };
    };

    services.picom = {
      enable = true;
      shadow = true;
      shadowExclude = [
        "class_g = 'Polybar'"
        "class_g = 'firefox' && (window_type = 'utility' || window_type = 'tooltip' || window_type = 'popup_menu')"
      ];
    };

    dog.programs.rofi = {
      enable = true;
      colors = {
        background = colors.Charcoal;
        text = "#FFF";
        active = colors.Blue-Munsell;
      };
    };

    dog.programs.polybar = {
      enable = true;
    };
  };
}
