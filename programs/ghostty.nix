{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.ghostty;
in
{
  options.dog.programs.ghostty = {
    enable = mkEnableOption "Ghostty";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (config.lib.nixGL.wrap inputs.ghostty.packages.x86_64-linux.ghostty)
    ];

    xdg.configFile = {
      "ghostty/config".text = ''
        command = ${lib.getExe pkgs.fish} -l
        theme = nightfox
        font-family = "VictorMono Nerd Font Propo"
        font-size = 11
        background-opacity = 0.95
        quit-after-last-window-closed = true
      '';
    };

    services.picom = {
      shadowExclude = [
        "class_g = 'com.mitchellh.ghostty' && window_type = 'menu'"
      ];
    };
  };
}
