{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.i3;
  colors = {
    Van-Dyke = "#3b312e";
    Khaki = "#c9b4a2";
    Blue-Munsell = "#2b8f9c";
    Cerulean = "#167b92";
    Charcoal = "#103e51";
    Caribbean-Current = "#006573";
    Lion = "#b99b81";
    Carolina-blue = "#85aecb";
    Alice-Blue = "#dee6ed";
    Umber = "#7b675a";
  };
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
          keybindings = lib.mkOptionDefault {
            # Mod1 == Alt key
            "Ctrl+Shift+Mod1+e" = "exec emacsclient -nc";
            "Ctrl+Shift+Mod1+f" = "exec firefox";
            "Ctrl+Shift+Mod1+k" = "exec keepassxc";
            "Ctrl+Shift+Mod1+m" = "exec spotify";
            "Ctrl+Shift+Mod1+t" = "exec ${cfg.terminal}";
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

    dog.programs.rofi = {
      enable = true;
      colors = {
        background = colors.Charcoal;
        text = "#FFF";
        active = colors.Blue-Munsell;
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

    systemd.user.services.polybar = {
      Service = {
        # ensure i3 is already running before starting
        ExecStartPre = "${getExe pkgs.i3} --get-socketpath";
      };
    };

    services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        i3Support = true;
        pulseSupport = true;
      };
      script = "polybar status &";
      settings = let
        transparent = "#00000000";
      in {
        "bar/status" = {
          bottom = true;
          dock = true;

          modules.left = "i3";
          modules.center = "";
          modules.right = "cpu memory fs wifi wired date";
          separator = " ";
          module.margin = 0;
          padding = 0;
          fixed.center = true;
          underline.size = 2;

          width = "100%:-30px";
          height = 27;
          offset.x = "15px";
          offset.y = "5px";

          font = [
            "VictorMono Nerd Font Propo:pixelsize=11;2"
            "VictorMono Nerd Font Propo:pixelsize=11:weight=bold;2"
          ];
          background = transparent;

          override.redirect = true;
          wm.restack = "i3";
        };
        "settings" = {
          pseudo.transparency = false;
          compositing = {
            background = "source";
            foreground = "over";
            overline = "over";
            underline = "over";
            border = "over";
          };
          format = {
            background = colors.Van-Dyke;
            foreground = colors.Alice-Blue;
            padding = 2;
          };
        };
        "module/i3" = let
          label = { text = "%name%"; background = colors.Charcoal; padding = 2; };
        in {
          type = "internal/i3";
          format.text = "<label-state> <label-mode>";
          format.padding = 0;
          label.focused = label // { font = 2; background = colors.Cerulean; };
          label.unfocused = label;
          label.visible = label;
          label.separator = {
            text = " ";
            padding = 0;
            background = transparent;
          };
        };
        "module/date" = {
          type = "internal/date";
          interval = 1;
          date = "%d.%m.%y";
          time = "%H.%M";
          label = "󰸘 %date% %time%";
        };
        "module/cpu" = {
          type = "internal/cpu";
          interval = 5;
          label = " %percentage-sum%%";
          bar.load = {
            width = 10;
            gradient = false;
            indicator = "\${bar.indicator}";
            fill = "\${bar.fill}";
            empty = "\${bar.empty}";
          };
        };
        "module/fs" = {
          type = "internal/fs";
          mount = [ "/" ];
          label-mounted = " %percentage_used%%";
        };
        "module/memory" = {
          type = "internal/memory";
          interval = 5;
          label = " %percentage_used%%";
        };
        "module/wifi" = {
          type = "internal/network";
          interface = "wlo1";
          interval = 5;
          format.disconnected = "󰖪";
          format.connected = "<ramp-signal> <label-connected>";
          label.connected = "%local_ip%  %downspeed%  %upspeed%";
          ramp.signal = [ "󰤯" "󰤟" "󰤢" "󰤥" "󰤨" ];
        };
      };
    };
  };
}
