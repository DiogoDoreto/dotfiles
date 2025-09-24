{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.emacs;

  dotfilesSymlink =
    pathInsideRepo:
    config.lib.file.mkOutOfStoreSymlink (config.dog.dotfilesPath + ("/" + pathInsideRepo));

  makeDoomMacroModules =
    moduleName: moduleCfg:
    let
      flags = concatStringsSep " " moduleCfg.flags;
      formattedModule = if flags == "" then moduleName else "(${moduleName} ${flags})";
    in
    if moduleCfg.enable then formattedModule else "";

  makeDoomMacroCategories =
    category:
    let
      moduleAttrs = attrsets.attrByPath [ category ] { } cfg.doom.init;
      modules = filter (m: m != "") (attrsets.mapAttrsToList makeDoomMacroModules moduleAttrs);
    in
    ''
      :${category}
        ${concatStringsSep "\n  " modules}
    '';

  doomInitMacroFile = ''
    ;;; init.el -*- lexical-binding: t; -*-
    (doom!
    ${concatStringsSep "\n" (map makeDoomMacroCategories cfg.doom.initOrder)}
    )

    ;; Ensure local packages are git repositories
    ;; Ref: https://github.com/doomemacs/doomemacs/commit/8cdddd87d948cb9ba561f2861f11af6fc74567d9
    (after! doom-straight
      (setq straight-vc-use-snapshot-installation nil))
  '';
in
{
  options.dog.programs.emacs = {
    enable = mkEnableOption "emacs + doom";

    whisperPackage = mkOption {
      type = types.package;
      default = pkgs.whisper-cpp;
    };

    doom.init =
      let
        doomModuleType = types.submodule {
          options = {
            enable = mkEnableOption "enable doom module";
            flags = mkOption {
              type = types.listOf types.str;
              default = [ ];
            };
          };
        };
      in
      mkOption {
        type = types.attrsOf (types.attrsOf doomModuleType);
        default = {
          completion = {
            corfu = {
              enable = true;
              flags = [
                "+orderless"
                "+dabbrev"
                "+icons"
              ];
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
            treemacs = {
              enable = true;
              flags = [ "+lsp" ];
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
              flags = [
                "+onsave"
                "+lsp"
              ];
            };
            parinfer.enable = true;
            snippets.enable = true;
            whitespace = {
              enable = true;
              flags = [ "+guess" "+trim" ];
            };
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
            debugger.enable = true;
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
              flags = [
                "+lsp"
                "+tree-sitter"
              ];
            };
            javascript = {
              enable = true;
              flags = [
                "+lsp"
                "+tree-sitter"
              ];
            };
            markdown.enable = true;
            nix = {
              enable = true;
              flags = [
                "+lsp"
                "+tree-sitter"
              ];
            };
            nix-extras.enable = true;
            org = {
              enable = true;
              flags = [ "+roam2" ];
            };
            qt.enable = true;
            scheme = {
              enable = true;
              flags = [ "+guile" ];
            };
            sh.enable = true;
            web = {
              enable = true;
              flags = [
                "+lsp"
                "+tree-sitter"
              ];
            };
            yaml.enable = true;
          };
          config = {
            default = {
              enable = true;
              flags = [
                "+bindings"
                "+smartparens"
                "+gnupg"
              ];
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
      startWithUserSession = true;
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
      "doom/config.el".source = dotfilesSymlink ".config/doom/config.el";
      "doom/packages.el".source = dotfilesSymlink ".config/doom/packages.el";

      "doom/snippets".source = dotfilesSymlink ".config/doom/snippets";

      "doom/dd".source = dotfilesSymlink ".config/doom/dd";

      "doom/modules/tools/ai/packages.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/packages.el";
      "doom/modules/tools/ai/gptel-oneshot.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/gptel-oneshot.el";
      "doom/modules/tools/ai/tools".source = dotfilesSymlink ".config/doom/modules/tools/ai/tools";
      "doom/modules/tools/ai/my-gptel-tools.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/my-gptel-tools.el";
      "doom/modules/tools/ai/config.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/config.el";
      "doom/modules/tools/ai/whisper-config.el".text = ''
        (setq whisper-cpp-models-directory "~/.local/share/whisper/models/")
        (setq whisper-cpp-directory "${cfg.whisperPackage}")
        (setq whisper-install-whispercpp nil)
      '';

      "doom/modules/tools/ai/whisper.el".source =
        let
          commit = "fc122657bfb8d5faf6aedaefdc1687193f456d1f";
        in
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/natrys/whisper.el/${commit}/whisper.el";
          sha256 = "sha256-jr3fl628fJYMEomT0aR9jrhqCxdVlLIz5umPic1mw3w=";
        };
    };
  };
}
