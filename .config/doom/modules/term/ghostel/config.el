;;; config.el -*- lexical-binding: t; -*-

(use-package ghostel
  :config
  ;; ghostel-module.so lives alongside ghostel.el in the straight repo dir;
  ;; ghostel.el finds it automatically via (file-name-directory load-file-name).
  (setq ghostel-shell "fish"
        ;; Module is always provided by Nix — never prompt to download or compile.
        ghostel-module-auto-install nil)

  (add-to-list 'doom-real-buffer-modes 'ghostel-mode)

  (add-hook 'doom-after-init-hook
            (lambda ()
              (map! :leader
                    :desc "Open ghostel term"      "o t" #'ghostel-project
                    :desc "Open ghostel term here" "o T" #'ghostel)))

  (map! :map ghostel-mode-map
        :desc "paste" :ni "C-S-v" #'ghostel-paste
        :n "C-p" (cmd! (ghostel-send-key "p" "ctrl"))
        :n "C-n" (cmd! (ghostel-send-key "n" "ctrl"))))

(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))
