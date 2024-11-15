{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    inputs.nur.hmModules.nur
    ../programs
  ];

  home = {
    stateVersion = "24.05";

    packages = with pkgs; [
      (nerdfonts.override { fonts = [ "VictorMono" ]; })
    ];

    sessionPath = [
      "${config.home.homeDirectory}/bin"
    ];
  };

  fonts.fontconfig = {
    enable = true;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.ssh-agent.enable = true;
}
