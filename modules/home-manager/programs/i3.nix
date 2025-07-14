{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.i3;
  colors = import ../../colorschemes/colors-beach.nix;
  wallpaper = ../../../wallpapers/wallhaven-x1xlmv.jpg;
in
{
  options.dog.programs.i3 = {
    enable = mkEnableOption "i3";

    terminal = mkOption {
      type = types.str;
      default = "ghostty";
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
          gaps.bottom = 30;
          workspaceAutoBackAndForth = true;
          defaultWorkspace = "workspace number 1";
          menu = config.dog.programs.rofi.command;
          keybindings = let
            playerctl = getExe pkgs.playerctl;
            pactl = "${pkgs.pulseaudio}/bin/pactl";
            sink = "\$(${pactl} get-default-sink)";
            xrandr = getExe pkgs.xorg.xrandr;
            lockscreen = pkgs.writeShellScript "lockscreen.sh" ''
              DIMENSION="$(${xrandr} --query | grep Screen | sed -r 's/^.*current ([0-9]+) x ([0-9]+).*$/\1x\2/')"
              ${pkgs.imagemagick}/bin/magick ${wallpaper} -resize "$DIMENSION^" -gravity SouthWest -extent "$DIMENSION" RGB:- | \
                i3lock -e --raw "$DIMENSION:rgb" --image /dev/stdin --color=000000
            '';
          in mkOptionDefault {
            # Mod1 == Alt key
            "Ctrl+Shift+Mod1+e" = "exec emacsclient -nc";
            "Ctrl+Shift+Mod1+f" = "exec firefox";
            "Ctrl+Shift+Mod1+k" = "exec keepassxc";
            "Ctrl+Shift+Mod1+m" = "exec pgrep spotify && [class=Spotify] focus || exec spotify";
            "Ctrl+Shift+Mod1+q" = "exec --no-startup-id ${lockscreen}";
            "XF86AudioPlay" = "exec ${playerctl} play-pause";
            "XF86AudioNext" = "exec ${playerctl} next";
            "XF86AudioPrev" = "exec ${playerctl} previous";
            "XF86AudioRaiseVolume" = "exec --no-startup-id ${pactl} set-sink-volume ${sink} +5%";
            "XF86AudioLowerVolume" = "exec --no-startup-id ${pactl} set-sink-volume ${sink} -5%";
            "XF86AudioMute" = "exec --no-startup-id ${pactl} set-sink-mute ${sink} toggle";
          };
          bars = [];
          startup = [
            {
              command = "${getExe pkgs.feh} --bg-fill ${wallpaper} --geometry=+0-300";
              always = true;
            }
            {
              command = "systemctl --user restart polybar";
              always = true;
              notification = false;
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
          window.commands = [
            { command = "border normal 1"; criteria = { class = "firefox"; }; }
          ];
        };
      };
    };

    services.picom = {
      enable = true;
      shadow = true;
      # `xprop` command to find class
      shadowExclude = [
        "class_g = 'Polybar'"
        "class_g = 'firefox' && (window_type = 'utility' || window_type = 'tooltip' || window_type = 'popup_menu')"
      ];
      extraArgs = [ "--crop-shadow-to-monitor" ];
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

    services.dunst = {
      enable = true;

      settings = {
        global = {
          origin = "bottom-right";
          font = "VictorMono Nerd Font 9";
          fullscreen = "pushback";
          background = "${colors.Alice-Blue}DD";
          foreground = colors.Van-Dyke;
          frame_color = colors.Van-Dyke;
          frame_width = 1;
          highlight = colors.Cerulean;
        };
        urgency_critical = {
          background = "#FFDAE4";
          frame_color = "#82202A";
        };
      };
    };
  };
}
