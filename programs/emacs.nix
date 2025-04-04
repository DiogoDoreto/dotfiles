{ config, lib, pkgs, inputs, ... }:

with lib;

let
  cfg = config.dog.programs.emacs;

  makeDoomMacroModules = moduleName: moduleCfg:
    let
      flags = concatStringsSep " " moduleCfg.flags;
      formattedModule = if flags == "" then moduleName else "(${moduleName} ${flags})";
    in if moduleCfg.enable then formattedModule else "";

  makeDoomMacroCategories = category:
    let
      moduleAttrs = attrsets.attrByPath [category] {} cfg.doom.init;
      modules = filter (m: m != "") (attrsets.mapAttrsToList makeDoomMacroModules moduleAttrs);
    in ''
      :${category}
        ${concatStringsSep "\n  " modules}
    '';

  doomInitMacroFile = ''
    ;;; init.el -*- lexical-binding: t; -*-
    (doom!
    ${concatStringsSep "\n" (map makeDoomMacroCategories cfg.doom.initOrder)}
    )
  '';
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

    doom.init = let
      doomModuleType = types.submodule {
        options = {
          enable = mkEnableOption "enable doom module";
          flags = mkOption {
            type = types.listOf types.str;
            default = [];
          };
        };
      };
    in mkOption {
      type = types.attrsOf (types.attrsOf doomModuleType);
      default = {
        completion = {
          corfu = {
            enable = true;
            flags = [ "+orderless" "+dabbrev" "+icons" ];
          };
          vertico = {
            enable = true;
            flags = [ "+icons" ];
          };
        };
        ui = {
          doom.enable = true;
          doom-dashboard.enable = true;
          hl-todo.enable = true;
          modeline.enable = true;
          ophints.enable = true;
          popup = {
            enable = true;
            flags = [ "+defaults" ];
          };
          smooth-scroll = {
            enable = true;
            flags = [ "+interpolate" ];
          };
          vc-gutter = {
            enable = true;
            flags = [ "+pretty" ];
          };
          vi-tilde-fringe.enable = true;
          window-select.enable = true;
          workspaces.enable = true;
        };
        editor = {
          evil = {
            enable = true;
            flags = [ "+everywhere" ];
          };
          file-templates.enable = true;
          fold.enable = true;
          format = {
            enable = true;
            flags = [ "+onsave" "+lsp" ];
          };
          parinfer.enable = true;
          snippets.enable = true;
        };
        emacs = {
          dired.enable = true;
          electric.enable = true;
          eww.enable = true;
          ibuffer = {
            enable = true;
            flags = [ "+icons" ];
          };
          undo.enable = true;
          vc.enable = true;
        };
        term = {
          eshell.enable = true;
          vterm.enable = true;
        };
        checkers = {
          syntax.enable = true;
        };
        tools = {
          direnv.enable = true;
          editorconfig.enable = true;
          eval = {
            enable = true;
            flags = [ "+overlay" ];
          };
          lookup = {
            enable = true;
            flags = [ "+dictionary" ];
          };
          lsp.enable = true;
          magit.enable = true;
          make.enable = true;
          tree-sitter.enable = true;
          lsp-extra.enable = true;
          ai.enable = true;
        };
        lang = {
          emacs-lisp.enable = true;
          json = {
            enable = true;
            flags = [ "+lsp" "+tree-sitter" ];
          };
          javascript = {
            enable = true;
            flags = [ "+lsp" "+tree-sitter" ];
          };
          markdown.enable = true;
          nix = {
            enable = true;
            flags = [ "+lsp" "+tree-sitter" ];
          };
          org = {
            enable = true;
            flags = [ "+roam2" ];
          };
          scheme = {
            enable = true;
            flags = [ "+guile" ];
          };
          sh.enable = true;
          web = {
            enable = true;
            flags = [ "+lsp" "+tree-sitter" ];
          };
          yaml.enable = true;
        };
        config = {
          default = {
            enable = true;
            flags = [ "+bindings" "+smartparens" "+gnupg" ];
          };
        };
      };
    };

    doom.initOrder = mkOption {
      type = types.listOf types.str;
      description = "The order in which the init categories will be written to the init.el file";
      default = [
        "input"
        "completion"
        "ui"
        "editor"
        "emacs"
        "term"
        "checkers"
        "tools"
        "os"
        "lang"
        "email"
        "app"
        "config"
      ];
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
      emacs.source = inputs.doomemacs;

      "doom/init.el".text = doomInitMacroFile;
      "doom/config-javascript.el".source = ../.config/doom/config-javascript.el;
      "doom/config.el" = {
        text = (readFile ../.config/doom/config.el) + "\n\n" + cfg.extraConfig;
        # try make it work later
        # onChange = "~/.config/emacs/bin/doom sync";
      };
      "doom/packages.el".text = (readFile ../.config/doom/packages.el)
        + "\n\n" + cfg.extraPackages;
      "doom/dd".source = ../.config/doom/dd;

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
