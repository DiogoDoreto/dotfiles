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
    home.packages = with pkgs; [
      delta # A syntax-highlighting pager for git
      eza # ls replacement
      htop
      libnotify
      nixd # nix lsp server
      nixfmt
      p7zip
      tldr
      unzip

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

      bottom.enable = true;

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
          tree = "eza --tree";
        };
        shellInitLast = ''
          ### Emacs' vterm integration ###

          function vterm_printf;
              if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
                  # tell tmux to pass the escape sequences through
                  printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
              else if string match -q -- "screen*" "$TERM"
                  # GNU screen (screen, screen-256color, screen-256color-bce)
                  printf "\eP\e]%s\007\e\\" "$argv"
              else
                  printf "\e]%s\e\\" "$argv"
              end
          end

          function vterm_prompt_end;
              vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
          end
          functions --copy fish_prompt vterm_old_fish_prompt
          function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
              # Remove the trailing newline from the original prompt. This is done
              # using the string builtin from fish, but to make sure any escape codes
              # are correctly interpreted, use %b for printf.
              printf "%b" (string join "\n" (vterm_old_fish_prompt))
              vterm_prompt_end
          end
        '';
      };

      fzf.enable = true;

      gh.enable = true;

      jq.enable = true;

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
