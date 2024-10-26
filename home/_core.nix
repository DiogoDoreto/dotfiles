{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    inputs.nur.hmModules.nur
    ../programs
  ];

  home = {
    stateVersion = "24.05";

    keyboard = {
      layout = "us";
      options = [ "compose:ralt" ];
    };

    packages = with pkgs; [
      (nerdfonts.override { fonts = [ "VictorMono" ]; })
    ];
  };

  fonts.fontconfig = {
    enable = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  systemd.user = {
    enable = true;
    startServices = "sd-switch";
    systemctlPath = "/usr/bin/systemctl";
  };

  services.ssh-agent.enable = true;
}
