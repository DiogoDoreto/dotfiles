{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.services.voice-commands;
in
{
  options.dog.services.voice-commands = {
    enable = lib.mkEnableOption "local Vosk voice commands with a system tray controller";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ../../flakes/voice-commands/package.nix { };
      defaultText = lib.literalExpression "pkgs.callPackage ../../flakes/voice-commands/package.nix { }";
      description = "Voice command runner and tray package.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "--execute" ];
      example = [
        "--execute"
        "--wake"
        "hey computer"
        "--player"
        "mpv"
      ];
      description = ''
        Command-line arguments for the tray and Vosk listener. Each argument is
        a separate list element; wake phrases containing spaces remain one argument.
        Include --execute to invoke actions, --mouse-grid for experimental
        Plasma Wayland mouse control with monitor selection, or --start-paused to start
        the tray without opening the microphone.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.voice-commands = {
      Unit = {
        Description = "Local voice commands and listening tray controller";
        After = [
          "graphical-session.target"
          "tray.target"
          "pipewire.service"
          "wireplumber.service"
        ];
        Wants = [
          "pipewire.service"
          "wireplumber.service"
        ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = lib.hm.strings.escapeSystemdExecArgs ([ (lib.getExe cfg.package) ] ++ cfg.extraArgs);
        Restart = "on-failure";
        RestartSec = "3s";
        TimeoutStopSec = "10s";
        KillMode = "mixed";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };

    xdg.desktopEntries.voice-commands = {
      name = "Voice Commands";
      comment = "Start the local voice command tray controller";
      exec = "${pkgs.systemd}/bin/systemctl --user start voice-commands.service";
      icon = "audio-input-microphone";
      terminal = false;
      categories = [ "Utility" ];
    };
  };
}
