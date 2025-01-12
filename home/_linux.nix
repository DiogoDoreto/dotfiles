{ config, lib, pkgs, inputs, ... }:

{
  home = rec {
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

    file = {
      ".Xresources".text = ''
        Xft.dpi: 110
        Xcursor*size: ${toString pointerCursor.size}
      '';
    };

    packages = with pkgs; [
      arandr # GUI for display management
      xclip
    ];

    sessionVariables = {
      SUDO_ASKPASS = let
        zenity = config.lib.nixGL.wrap pkgs.zenity;
      in pkgs.writeShellScript "zenity_askpass.sh" ''
        ${lib.getExe zenity} --password --title="Sudo password"
      '';
    };
  };

  targets.genericLinux.enable = true;

  nixGL.packages = inputs.nixgl.packages;

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
}
