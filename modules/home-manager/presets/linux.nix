{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.presets.linux;
in
{
  options.dog.presets.linux = {
    enable = mkEnableOption "presets.linux";
  };

  config = mkIf cfg.enable {
    home = {
      keyboard = {
        layout = "us";
        options = [ "compose:ralt" ];
      };

      pointerCursor = {
        name = "Nordzy-cursors-white";
        package = pkgs.nordzy-cursor-theme;
        size = 32;
        gtk.enable = true;
      };

      sessionVariables = {
        SUDO_ASKPASS = let
          zenity = config.lib.nixGL.wrap pkgs.zenity;
        in pkgs.writeShellScript "zenity_askpass.sh" ''
        ${lib.getExe zenity} --password --title="Sudo password"
      '';
      };
    };

    targets.genericLinux.enable = true;

    systemd.user = {
      enable = true;
      startServices = "sd-switch";
      systemctlPath = "/usr/bin/systemctl";
    };

    # Automatically hide mouse cursor
    services.unclutter = {
      enable = true;
      extraOptions = [ "ignore-scrolling" ];
    };
  };
}
