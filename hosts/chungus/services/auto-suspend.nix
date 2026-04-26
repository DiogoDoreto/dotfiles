{
  config,
  pkgs,
  lib,
  ...
}:

let
  chungusActivityCheck = pkgs.writeShellScript "chungus-activity-check" ''
    # Exit 0 = ACTIVE (block suspend), exit 1 = idle (allow suspend)
    if nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits \
        2>/dev/null | awk '{if ($1+0 > 5) exit 0} END {exit 1}'; then
      exit 0
    fi
    if pgrep -x llama-server > /dev/null 2>&1; then
      exit 0
    fi
    exit 1
  '';

  chungusScreenSaverCheck = pkgs.writeShellScript "chungus-screensaver-check" ''
    # Exit 0 = ACTIVE (block suspend), exit 1 = idle (allow suspend)
    # GetActive returns "true" when screen is locked, "false" when a user is present.
    # Must run qdbus as dog because the D-Bus session daemon rejects root connections.
    uid=$(id -u dog 2>/dev/null) || exit 1
    socket=/run/user/$uid/bus
    [ -S "$socket" ] || exit 1
    result=$(runuser -u dog -- env \
      DISPLAY=:0 DBUS_SESSION_BUS_ADDRESS=unix:path=$socket \
      ${pkgs.kdePackages.qttools}/bin/qdbus \
      org.freedesktop.ScreenSaver /ScreenSaver \
      org.freedesktop.ScreenSaver.GetActive 2>/dev/null)
    [ "$result" = "false" ] && exit 0
    exit 1
  '';

  # Re-check activity immediately before suspending. If something became active
  # between the last poll and now, exit 1 so autosuspend aborts and resets the
  # idle timer rather than racing into S3 under load.
  preSuspend = pkgs.writeShellScript "chungus-pre-suspend" ''
    if ${chungusActivityCheck}; then
      exit 1
    fi
    systemctl suspend
  '';
in
{
  # ethtool WoL is cleared on full power cycles, so arm it at boot and before sleep.
  systemd.services.wol-enable = {
    description = "Arm Wake-on-LAN magic-packet on enp4s0";
    wantedBy = [
      "multi-user.target"
      "sleep.target"
    ];
    before = [ "sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.ethtool}/bin/ethtool -s enp4s0 wol g";
      RemainAfterExit = true;
    };
  };

  services.autosuspend = {
    enable = true;
    settings = {
      interval = 60;
      idle_time = 600;
      suspend_cmd = toString preSuspend;
    };
    checks = {
      GpuActivity = {
        class = "ExternalCommand";
        command = toString chungusActivityCheck;
      };
      AiPortConnections = {
        class = "ActiveConnection";
        ports = "8080, 9090";
      };
      RemoteSSH = {
        class = "Users";
        name = ".*";
        terminal = "pts/.*";
        host = "[0-9].*";
      };
      ScreenSaverInactive = {
        class = "ExternalCommand";
        command = toString chungusScreenSaverCheck;
      };
    };
  };

  systemd.services.autosuspend.path = lib.mkAfter [
    pkgs.procps
    pkgs.gawk
    config.hardware.nvidia.package.bin
    pkgs.kdePackages.qttools
  ];
}
