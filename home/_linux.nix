{ config, lib, pkgs, inputs, ... }:

rec {
  imports = [
    "${inputs.home-manager-unstable}/modules/misc/nixgl.nix"
  ];

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

    file = {
      ".Xresources".text = ''
        Xft.dpi: 110
        Xcursor*size: ${toString home.pointerCursor.size}
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
