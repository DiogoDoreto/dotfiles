{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.podman;
in {
  options.dog.programs.podman = {
    enable = mkEnableOption "Podman";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      podman
      pods # podman GUI
      # podman dependencies:
      qemu
      virtiofsd
    ];

    services.podman = {
      enable = true;
      settings = {
        policy = {
          default = [{ type = "insecureAcceptAnything"; }];
        };
      };
    };
  };
}
