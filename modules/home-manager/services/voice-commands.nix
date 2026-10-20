{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dog.services.voice-commands;
  structuredArgs = [
    (if cfg.execute then "--execute" else "--no-execute")
    "--wake"
    cfg.wakePhrase
    (if cfg.features.media.enable then "--media" else "--no-media")
    (if cfg.features.scroll.enable then "--scroll" else "--no-scroll")
    (if cfg.features.mouseGrid.enable then "--mouse-grid" else "--no-mouse-grid")
  ];
  reservedArgs = [
    "--execute"
    "--no-execute"
    "--wake"
    "--media"
    "--no-media"
    "--scroll"
    "--no-scroll"
    "--mouse-grid"
    "--no-mouse-grid"
    "--grid-stdin"
    "--"
  ];
in
{
  options.dog.services.voice-commands = {
    enable = lib.mkEnableOption "local Vosk voice commands with a system tray controller";

    execute = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Invoke actions rather than logging previews only.";
    };

    wakePhrase = lib.mkOption {
      type = lib.types.str;
      default = "hey computer";
      description = "English wake phrase; words must exist in the selected Vosk model.";
    };

    features = {
      media.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Recognize and dispatch MPRIS media commands.";
      };
      scroll.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable the repeated up/down scroll command group.";
      };
      mouseGrid.enable = lib.mkEnableOption "experimental Plasma Wayland mouse-grid commands";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.callPackage ../../flakes/voice-commands/package.nix { };
      defaultText = lib.literalExpression "pkgs.callPackage ../../flakes/voice-commands/package.nix { }";
      description = "Voice command runner and tray package.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "--player"
        "mpv"
      ];
      description = ''
        Command-line arguments for the tray and Vosk listener. Each argument is
        a separate list element; wake phrases containing spaces remain one argument.
        Use these for diagnostics or unusual settings, such as --start-paused,
        --player, or --verbose. Execution, wake phrase, and feature flags are
        reserved for the structured options; --grid-stdin is standalone-only.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.all (
          arg: !(lib.elem (builtins.head (lib.splitString "=" arg)) reservedArgs)
        ) cfg.extraArgs;
        message = "voice-commands.extraArgs cannot contain execution, wake, feature flags, --grid-stdin, or --; use the structured options.";
      }
    ];
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
        ExecStart = lib.hm.strings.escapeSystemdExecArgs (
          [ (lib.getExe cfg.package) ] ++ cfg.extraArgs ++ structuredArgs
        );
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
