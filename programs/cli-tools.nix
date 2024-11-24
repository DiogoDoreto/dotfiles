{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.cli-tools;
in
{
  options.dog.programs.cli-tools = {
    enable = mkEnableOption "Core CLI tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      delta # A syntax-highlighting pager for git
      eza # ls replacement
      htop
      nil # nix lsp server
      tldr

      # build tools
      cmake
      gcc
      gnumake
      libtool
    ];

    programs = {
      atuin = {
        enable = true;
        enableFishIntegration = true;
      };

      bash.enable = true;

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fd.enable = true;

      fish = {
        enable = true;
        shellAbbrs = {
          e = "emacsclient -nc";
          la = "eza -l --icons --git -a";
          ll = "eza -l --icons --git --git-ignore";
        };
      };

      fzf.enable = true;

      gh.enable = true;

      ripgrep.enable = true;

      starship = {
        enable = true;
        enableFishIntegration = true;
      };

      zoxide = {
        enable = true;
        enableFishIntegration = true;
      };
    };
  };
}
