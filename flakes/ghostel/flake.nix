{
  description = "ghostel — Emacs terminal emulator powered by libghostty-vt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # ── Source fetches ────────────────────────────────────────────────────
        # Elisp source only — no submodules needed; the native module is built
        # separately below. Bump rev + hash together when updating ghostel.
        ghostel-src = pkgs.fetchFromGitHub {
          owner = "dakra";
          repo = "ghostel";
          rev = "0f6038896e27910750a21c5fc16334034ad2c830";
          hash = "sha256-eveu/6/PTas+NUTmteR8DuVOhUNRCQxTpRSbRfQPFeQ=";
        };

        # ── Native module build chain ─────────────────────────────────────────
        libghostty-vt = pkgs.callPackage ./libghostty-vt.nix { };

        ghostel-module = pkgs.callPackage ./ghostel-module.nix {
          inherit ghostel-src libghostty-vt;
          emacs = pkgs.emacs30;
        };

        # ── Combined package ──────────────────────────────────────────────────
        # Merges the elisp source with the compiled .so into a single directory.
        # This is what straight.el's repos/ghostel/ symlink points to:
        #   - ghostel.el and supporting .el files
        #   - etc/ directory (shell integration scripts for bash/zsh/fish)
        #   - ghostel-module.so alongside the .el files (ghostel.el finds it
        #     via (file-name-directory load-file-name) at require time)
        ghostel-package = pkgs.runCommand "ghostel-package" { } ''
          mkdir -p $out

          # Elisp files
          cp ${ghostel-src}/*.el $out/

          # Shell integration scripts
          cp -r ${ghostel-src}/etc $out/

          # Native module — must live alongside ghostel.el
          cp ${ghostel-module}/lib/ghostel-module.so $out/
        '';

      in
      {
        packages = {
          inherit
            libghostty-vt
            ghostel-module
            ghostel-package
            ;
          default = ghostel-package;
        };

        # Overlay injects ghostel-package into pkgs so emacs.nix can reference
        # pkgs.ghostel-package without knowing the flake structure.
        overlays.default = _final: _prev: {
          inherit ghostel-package;
        };
      }
    );
}
