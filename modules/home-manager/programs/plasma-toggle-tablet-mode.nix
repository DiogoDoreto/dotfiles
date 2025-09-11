{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.dog.programs.plasma-toggle-tablet-mode;
in
{
  options.dog.programs.plasma-toggle-tablet-mode = {
    enable = lib.mkEnableOption "plasma-toggle-tablet-mode tool";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.plasma-toggle-tablet-mode;
      description = "The plasma-toggle-tablet-mode package to use.";
    };

    devices = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "AT Translated Set 2 keyboard"
      ];
      description = "List of device names to disable in tablet mode.";
    };

    createDesktopEntry = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to create a desktop entry for plasma-toggle-tablet-mode.";
    };

  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."plasma-toggle-tablet-mode/config.json".text = builtins.toJSON {
      devices = cfg.devices;
    };

    xdg.desktopEntries = lib.mkIf cfg.createDesktopEntry {
      plasma-toggle-tablet-mode = {
        name = "Plasma Toggle Tablet Mode";
        exec = "${lib.getExe cfg.package} toggle";
        icon = "input-tablet";
        type = "Application";
        categories = [ "Utility" ];
        comment = "Toggle tablet mode & disable devices";
      };
    };
  };
}
