{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.cli-tools;
in
{
  options.dog.programs.cli-tools = {
    enable = mkEnableOption "Core CLI tools";
  };

  config = mkIf cfg.enable {
    # TOML literal strings preserve the escaped dollar for Starship's format parser.
    xdg.configFile."starship/bash.toml".text = ''
      [character]
      success_symbol = '[\$](bold green)'
      error_symbol = '[\$](bold red)'
    '';

    home.packages = with pkgs; [
      delta # A syntax-highlighting pager for git
      eza # ls replacement
      htop
      libnotify
      nodejs
      nixd # nix lsp server
      nixfmt
      nix-prefetch-git
      p7zip
      tldr
      unzip

      # build tools
      cmake
      gcc
      gnumake
      libtool

      # python
      python3
      python3Packages.pip
      uv
    ];

    programs = {
      atuin = {
        enable = true;
        enableFishIntegration = true;
      };

      bash = {
        enable = true;
        initExtra = ''
          export STARSHIP_CONFIG="${config.xdg.configHome}/starship/bash.toml"
        '';
      };

      bottom.enable = true;

      direnv = {
        enable = true;
        nix-direnv.enable = true;
      };

      fd.enable = true;

      fish = {
        enable = true;
        shellInit = ''
          # Reset the config inherited when Fish is launched from Bash.
          set -gx STARSHIP_CONFIG "${config.programs.starship.configPath}"
        '';
        shellAbbrs = {
          e = "emacsclient -nc";
          la = "eza -l --icons --git -a";
          ll = "eza -l --icons --git --git-ignore";
          tree = "eza --tree";
        };
      };

      fzf = {
        enable = true;
        # disable history widget in favor of Atuin
        historyWidget.command = "";
      };

      gh.enable = true;

      jq.enable = true;

      ripgrep.enable = true;

      starship = {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
      };

      tmux = {
        enable = true;
        keyMode = "vi";
        mouse = true;
        baseIndex = 1;
        escapeTime = 0;
        historyLimit = 10000;
        focusEvents = true;
        sensibleOnTop = true;
        terminal = "tmux-256color";
      };

      zoxide = {
        enable = true;
        enableFishIntegration = true;
      };
    };
  };
}
