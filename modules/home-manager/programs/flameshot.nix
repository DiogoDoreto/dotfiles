{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.flameshot;
  colors = import ../../colorschemes/colors-beach.nix;
in {
  options.dog.programs.flameshot = {
    enable = mkEnableOption "FlameShot";
  };

  config = mkIf cfg.enable {
    services.flameshot = {
      enable = true;
      settings = {
        General = {
          userColors = strings.concatStringsSep ", " [
            "picker"
            "crimson"
            "deep sky blue"
            "yellow green"
            "white"
            "black"
          ];
          uiColor = colors.Cerulean;
          drawColor = "crimson";
        };
      };
    };

    xsession.windowManager.i3.config.keybindings = let
      flameshot = getExe pkgs.flameshot;
    in lib.mkOptionDefault {
      "Print" = "exec --no-startup-id ${flameshot} gui";
      "Shift+Print" = "exec --no-startup-id ${flameshot} gui --delay 3000";
    };
  };
}
