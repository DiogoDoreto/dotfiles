{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.presets.xorg;
in
{
  options.dog.presets.xorg = {
    enable = mkEnableOption "presets.xorg";
  };

  config = mkIf cfg.enable {
    dog.presets.linux.enable = true;

    home = {
      file = {
        ".Xresources".text = ''
        Xft.dpi: 110
        Xcursor*size: ${toString config.home.pointerCursor.size}
      '';
      };

      packages = with pkgs; [
        arandr # GUI for display management
        xclip
      ];
    };
  };
}
