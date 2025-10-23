{ config, pkgs-unstable, inputs, ... }:

{
  imports = [
    ./dog.nix
    ./presets
    ./programs
  ];

  home = {
    stateVersion = "25.05";

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

  # Ensure shell usage of nixpkgs is the same version of the current build
  nix.registry.nixpkgs.flake = inputs.nixpkgs;

  services.ssh-agent.enable = true;
}
