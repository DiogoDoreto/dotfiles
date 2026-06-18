{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.polybar;
  colors = import ../../colorschemes/colors-beach.nix;
in
{
  options.dog.programs.polybar = {
    enable = mkEnableOption "Polybar";

    wiredInterface = mkOption {
      type = types.str;
      default = "eth0";
    };
    wirelessInterface = mkOption {
      type = types.str;
      default = "wlo1";
    };
  };

  config = mkIf cfg.enable {
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
      script =
        let
          grep = "${pkgs.gnugrep}/bin/grep";
          cut = "${pkgs.coreutils}/bin/cut";
          head = "${pkgs.coreutils}/bin/head";
          wc = "${pkgs.coreutils}/bin/wc";
        in
        ''
          MONITORS="$(polybar -m)"
          if [ "$(echo "$MONITORS" | ${wc} -l)" -eq "1" ]; then
              polybar primary &
          else
              function monitor_name { ${cut} -d: -f1; }
              MONITOR="$(echo "$MONITORS" | ${grep} primary | monitor_name)" polybar primary &
              MONITOR="$(echo "$MONITORS" | ${grep} --invert-match primary | ${head} -n1 | monitor_name)" polybar secondary &
          fi
        '';
      settings =
        let
          transparent = "#00000000";
          defaultBar = {
            bottom = true;
            dock = true;
            monitor = "\${env:MONITOR:}";

            modules.left = "i3";
            modules.center = "";
            modules.right = "date";
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
        in
        {
          "bar/primary" = defaultBar // {
            modules = defaultBar.modules // {
              center = "spotify";
              right = "audio battery cpu memory fs eth0 wifi wired date tray";
            };
          };
          "bar/secondary" = defaultBar;
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
          "module/i3" =
            let
              label = {
                text = "%name%";
                background = colors.Charcoal;
                padding = 2;
              };
            in
            {
              type = "internal/i3";
              pin.workspaces = true;
              index.sort = true;
              format.text = "<label-state> <label-mode>";
              format.padding = 0;
              label.focused = label // {
                font = 2;
                background = colors.Cerulean;
              };
              label.unfocused = label;
              label.visible = label // {
                font = 2;
              };
              label.urgent = label // {
                background = colors.Crimson;
              };
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
          "module/battery" = {
            type = "internal/battery";
            format.charging = "<ramp-capacity> <label-charging>";
            label.charging = "󰚥 %percentage%% %consumption%w";
            format.discharging = "<ramp-capacity> <label-discharging>";
            label.discharging = "%percentage%% %consumption%w";
            label.full = "󱟢";
            ramp.capacity = [
              "󰂎"
              "󰁺"
              "󰁻"
              "󰁼"
              "󰁽"
              "󰁾"
              "󰁿"
              "󰂀"
              "󰂁"
              "󰂂"
            ];
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
          "module/eth0" = {
            type = "internal/network";
            interface = cfg.wiredInterface;
            interval = 5;
            format.disconnected = "";
            format.connected = "<label-connected>";
            label.connected = "󰱔 %local_ip%  %downspeed%  %upspeed%";
          };
          "module/wifi" = {
            type = "internal/network";
            interface = cfg.wirelessInterface;
            interval = 5;
            format.disconnected = "";
            format.connected = "<ramp-signal> <label-connected>";
            label.connected = "%local_ip%  %downspeed%  %upspeed%";
            ramp.signal = [
              "󰤯"
              "󰤟"
              "󰤢"
              "󰤥"
              "󰤨"
            ];
          };
          "module/spotify" = {
            # Shows current playing musing info only when playing. Hides when paused/closed.
            type = "custom/script";
            tail = true;
            click.left = "${pkgs.i3}/bin/i3-msg '[class=Spotify] focus'";
            format.prefix = "󰝚  ";
            exec =
              let
                playerctl = "${getExe pkgs.playerctl} -p spotify,firefox";
                zscroll = getExe pkgs.zscroll;
                statusCmd = pkgs.writeShellScript "player-status.sh" ''
                  status="$(${playerctl} status 2>/dev/null)"
                  if [[ "$status" = "Playing" ]]; then
                    ${playerctl} metadata --format "{{ artist }} - {{ title }}"
                  fi
                '';
                scrollCmd = pkgs.writeShellScript "player-scroll.sh" ''
                  ${zscroll} -u true '${statusCmd}' &
                  wait
                '';
              in
              toString scrollCmd;
          };
          "module/audio" = {
            type = "internal/pulseaudio";
            reverse.scroll = true;
            format.volume = "󰕾 <label-volume>";
            label.muted = "󰝟";
          };
          "module/tray" = {
            type = "internal/tray";
            tray.background = colors.Van-Dyke;
            tray.spacing = 10;
          };
        };
    };
  };
}
