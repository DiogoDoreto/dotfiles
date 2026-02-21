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
      flags = concatStringsSep " " (moduleCfg.flags or [ ]);
      formattedModule = if flags == "" then moduleName else "(${moduleName} ${flags})";
    in
    if moduleCfg.enable then formattedModule else "";

  makeDoomMacroCategories =
    doomInit: category:
    let
      moduleAttrs = attrsets.attrByPath [ category ] { } doomInit;
      modules = filter (m: m != "") (attrsets.mapAttrsToList makeDoomMacroModules moduleAttrs);
    in
    ''
      :${category}
        ${concatStringsSep "\n  " modules}
    '';

  doomInitMacroFile = doomInit: ''
    ;;; init.el -*- lexical-binding: t; -*-
    (doom!
    ${concatStringsSep "\n" (map (makeDoomMacroCategories doomInit) cfg.doom.initOrder)}
    )

    ;; Ensure local packages are git repositories
    ;; Ref: https://github.com/doomemacs/doomemacs/commit/8cdddd87d948cb9ba561f2861f11af6fc74567d9
    (with-eval-after-load 'doom-straight
      (setq straight-vc-use-snapshot-installation nil))
  '';

  defaultDoomInit = {
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
        flags = [
          "+guess"
          "+trim"
        ];
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
      elixir = {
        enable = true;
        flags = [
          "+lsp"
          "+tree-sitter"
        ];
      };
      elixir-extra.enable = true;
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
      rest = {
        enable = true;
        flags = [ "+jq" ];
      };
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
    app = {
      rss.enable = true;
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
in
{
  options.dog.programs.emacs = {
    enable = mkEnableOption "emacs + doom";

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
        default = defaultDoomInit;
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
      ripgrep

      # spell checking
      wordnet
      (enchant.overrideAttrs (old: rec {
        # jinx needs enchant.h to compile itself
        # nixpkgs version 2.6.9 does not include it in the tarball
        # latest version 2.8.12 fails to build
        # version 2.7.3 has the header but it is not being placed on the /nix/store output (FIXME)
        version = "2.7.3";
        src = fetchurl {
          url = "https://github.com/rrthomas/${old.pname}/releases/download/v${version}/${old.pname}-${version}.tar.gz";
          hash = "sha256-/mrUy+jHG5OE/975Yr5S1NK9Xr+2NRQ1uzkFQ9T3ix4=";
        };
      }))
      hunspellDicts.pt_BR
      hunspellDicts.es_ES
      hunspellDicts.en_US
      aspellDicts.en-computers
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

      "doom/init.el".text = doomInitMacroFile (attrsets.recursiveUpdate defaultDoomInit cfg.doom.init);
      "doom/config.el".source = dotfilesSymlink ".config/doom/config.el";
      "doom/packages.el".source = dotfilesSymlink ".config/doom/packages.el";

      "doom/snippets".source = dotfilesSymlink ".config/doom/snippets";

      "doom/dd".source = dotfilesSymlink ".config/doom/dd";

      "doom/modules/lang/elixir-extra".source = dotfilesSymlink ".config/doom/modules/lang/elixir-extra";

      "doom/modules/tools/ai/packages.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/packages.el";
      "doom/modules/tools/ai/gptel-oneshot.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/gptel-oneshot.el";
      "doom/modules/tools/ai/gptel-prompts.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/gptel-prompts.el";
      "doom/modules/tools/ai/tools".source = dotfilesSymlink ".config/doom/modules/tools/ai/tools";
      "doom/modules/tools/ai/my-gptel-tools.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/my-gptel-tools.el";
      "doom/modules/tools/ai/config.el".source =
        dotfilesSymlink ".config/doom/modules/tools/ai/config.el";
    };
  };
}
