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
      qemu # podman dependency
    ];

    home.file = {
      ".config/containers/policy.json".text = strings.toJSON {
        default = [{ type = "insecureAcceptAnything"; }];
      };
    };
  };
}
