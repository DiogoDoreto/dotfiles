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
