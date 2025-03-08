{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.emacs;
in
{
  options.dog.programs.emacs = {
    enable = mkEnableOption "emacs + doom";

    extraConfig = mkOption {
      type = types.lines;
      default = "";
    };

    extraPackages = mkOption {
      type = types.lines;
      default = "";
    };

    whisperPackage = mkOption {
      type = types.package;
      default = pkgs.openai-whisper-cpp;
    };
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs30;
    };

    services.emacs = {
      enable = true;
      defaultEditor = true;
      startWithUserSession  = true;
    };

    home.packages = with pkgs; [
      sqlite # for org-roam
      ffmpeg # for whisper package
      ripgrep
      wordnet
    ];

    home.sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
    ];

    home.sessionVariables = {
      DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
      DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };

    xdg.configFile = {
      "doom/init.el".source = ../.config/doom/init.el;
      "doom/config-javascript.el".source = ../.config/doom/config-javascript.el;
      "doom/config.el" = {
        text = (readFile ../.config/doom/config.el) + "\n\n" + cfg.extraConfig;
        # try make it work later
        # onChange = "~/.config/emacs/bin/doom sync";
      };
      "doom/packages.el".text = (readFile ../.config/doom/packages.el)
        + "\n\n" + cfg.extraPackages;
      "doom/dd".source = ../.config/doom/dd;

      emacs.source = pkgs.fetchFromGitHub {
        owner = "doomemacs";
        repo = "doomemacs";
        rev = "8846d151814ebbf7fb90d9d5dd16cd737257408e";
        hash = "sha256-3a1cfxzaM/jvrrODgYdxyji9KN0rQZjPJzgPFNRiBIw=";
      };

      "doom/modules/tools/lsp-extra/config.el".source = ../.config/doom/modules/tools/lsp-extra/config.el;

      "doom/modules/tools/ai/packages.el".source = ../.config/doom/modules/tools/ai/packages.el;
      "doom/modules/tools/ai/config.el".text = (readFile ../.config/doom/modules/tools/ai/config.el) + ''
        (defun whisper--find-whispercpp-main () "" "${getExe cfg.whisperPackage}")
        (setq whisper-install-whispercpp nil)
      '';

      "doom/modules/tools/ai/whisper.el".source = let
        commit = "fc122657bfb8d5faf6aedaefdc1687193f456d1f";
      in pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/natrys/whisper.el/${commit}/whisper.el";
        sha256 = "sha256-jr3fl628fJYMEomT0aR9jrhqCxdVlLIz5umPic1mw3w=";
      };
    };
  };
}
