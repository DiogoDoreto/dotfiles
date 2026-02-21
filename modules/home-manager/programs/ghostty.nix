{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.ghostty;
in
{
  options.dog.programs.ghostty = {
    enable = mkEnableOption "Ghostty";
  };

  config = mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        command = "${lib.getExe pkgs.fish} -l";
        theme = "Nightfox";
        window-theme = "ghostty";
        font-family = "VictorMono Nerd Font Propo";
        font-size = 11;
        background-opacity = 0.95;
        quit-after-last-window-closed = true;
      };
    };

    services.picom = {
      shadowExclude = [
        "class_g = 'com.mitchellh.ghostty' && window_type = 'menu'"
      ];
    };
  };
}
