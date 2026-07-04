{
  config,
  lib,
  dog-lib,
  pkgs,
  inputs,
  ...
}:

with lib;

let
  cfg = config.dog.programs.emacs;
  finalDoomModules = attrsets.recursiveUpdate defaultDoomInit cfg.doom.init;

  inherit (dog-lib) dotfilesSymlink;

  aspell' = pkgs.aspellWithDicts (
    dicts: with dicts; [
      en
      en-computers
    ]
  );
  enchant' = pkgs.enchant_2.override { aspell = aspell'; };

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

    ${cfg.doom.preInit}

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
      dashboard.enable = true;
      doom.enable = true;
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
      ghostel.enable = false;
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
      markdown = {
        enable = true;
        flags = [
          "+grip"
        ];
      };
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
        flags = [ "+dragndrop" ];
      };
      org-extra.enable = true;
      rest = {
        enable = true;
        flags = [ "+jq" ];
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

    doom.preInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Elisp snippets to insert at the top of init.el, before the (doom!) macro.
        Multiple definitions are concatenated, so additional modules can append
        their own setup without overriding existing entries.
      '';
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
    dog.programs.emacs.doom.preInit = ''
      ;; Restore evil-collection's default before Doom initializes evil-collection.
      (setq evil-collection-repl-submit-state 'normal)

      (setq parinfer-rust-library "${pkgs.parinfer-rust-emacs}/lib/libparinfer_rust.so")
    '';

    programs.emacs = {
      enable = true;
      package = pkgs.emacs31;
      extraPackages =
        epkgs:
        let
          jinxWithAspellEnchant = epkgs.jinx.overrideAttrs (old: {
            buildInputs = lib.remove pkgs.enchant_2 (old.buildInputs or [ ]) ++ [
              enchant'
            ];
          });
        in
        [ jinxWithAspellEnchant ]
        ++ lib.optionals finalDoomModules.term.vterm.enable [ epkgs.vterm ]
        ++ lib.optionals finalDoomModules.term.ghostel.enable [
          epkgs.ghostel
          epkgs.evil-ghostel
        ];
    };

    services.emacs = {
      enable = true;
      defaultEditor = true;
      startWithUserSession = true;
    };

    home.packages = with pkgs; [
      ripgrep

      # spell checking
      wordnet
      hunspellDicts.pt_BR
      hunspellDicts.es_ES
      hunspellDicts.en_US
      enchant'
      aspell'
    ];

    home.file = {
      ".aspell.conf".text = ''
        data-dir ${aspell'}/lib/aspell
        dict-dir ${aspell'}/lib/aspell
        master en_US
        # TODO en-computers does not seem to be recognized
        add-extra-dicts en-computers.rws
        extra-dicts en-computers.rws
      '';
    };

    home.sessionPath = [
      "${config.xdg.configHome}/emacs/bin"
    ];

    home.sessionVariables = {
      DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
      DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };

    xdg.configFile = {
      emacs.source = inputs.doomemacs;

      "doom/init.el".text = doomInitMacroFile finalDoomModules;
      "doom/config.el".source = dotfilesSymlink ".config/doom/config.el";
      "doom/packages.el".source = dotfilesSymlink ".config/doom/packages.el";

      "doom/snippets".source = dotfilesSymlink ".config/doom/snippets";

      "doom/dd".source = dotfilesSymlink ".config/doom/dd";

      "doom/modules".source = dotfilesSymlink ".config/doom/modules";
    };
  };
}
