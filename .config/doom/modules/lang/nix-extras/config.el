;;; config.el --- Extra nix goodies -*- lexical-binding: t; -*-

(after! nix-mode
  (setq +lookup-provider-url-alist
        (append +lookup-provider-url-alist
                '(("Nix Packages" "https://search.nixos.org/packages?query=%s")
                  ("Nix Options" "https://search.nixos.org/options?query=%s")
                  ("Nix Wiki" "https://nixos.wiki/index.php?search=%s&go=Go&fulltext=1"))))

  (defun +nix-extras/send-pkgs-to-nix-repl ()
    "Send `pkgs = import <nixpkgs> {}` to the active REPL."
    (interactive)
    (comint-send-string (get-buffer-process (current-buffer))
                        "pkgs = import <nixpkgs> {}\n"))

  (map! :map nix-repl-mode-map
        :localleader
        :desc "Send pkgs import" "p" #'+nix-extras/send-pkgs-to-nix-repl))
