{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../programs
  ];

  home = {
    stateVersion = "24.11";

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
