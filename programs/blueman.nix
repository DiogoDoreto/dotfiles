{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.blueman;
in {
  options.dog.programs.blueman = {
    enable = mkEnableOption "blueman";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      blueman
    ];

    services.blueman-applet.enable = true;
  };
}
