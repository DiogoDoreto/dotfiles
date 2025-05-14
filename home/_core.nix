{ config, pkgs-unstable, ... }:

{
  imports = [
    ../programs
  ];

  home = {
    stateVersion = "24.11";

    packages = with pkgs-unstable; [
      nerd-fonts.victor-mono
      nerd-fonts.symbols-only
    ];

    sessionPath = [
      "${config.home.homeDirectory}/bin"
    ];
  };

  fonts.fontconfig.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.ssh-agent.enable = true;
}
