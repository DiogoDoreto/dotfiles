{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.polybar;
  colors = import ../utils/colors-beach.nix;
in
{
  options.dog.programs.polybar = {
    enable = mkEnableOption "Polybar";
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
      script = "polybar status &";
      settings = let
        transparent = "#00000000";
      in {
        "bar/status" = {
          bottom = true;
          dock = true;

          modules.left = "i3";
          modules.center = "spotify";
          modules.right = "audio cpu memory fs wifi wired date";
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
        "module/spotify" = {
          # Shows current playing musing info only when playing. Hides when paused/closed.
          type = "custom/script";
          tail = true;
          click.left = "${pkgs.i3}/bin/i3-msg '[class=Spotify] focus'";
          format.prefix = "  ";
          exec = let
            playerctl = "${getExe pkgs.playerctl} -p spotify";
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
          in toString scrollCmd;
        };
        "module/audio" = {
          type = "internal/pulseaudio";
          reverse.scroll = true;
          format.volume = "󰕾 <label-volume>";
          label.muted = "󰝟";
        };
      };
    };
  };
}
