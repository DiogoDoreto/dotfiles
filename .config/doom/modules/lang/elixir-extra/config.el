;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Diogo Doreto
;;
;; Author: Diogo Doreto
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(use-package alchemist
  :defer t
  :hook elixir-ts-mode
  :config
  (set-popup-rules! '(("^\\*alchemist-info-mode\\*$" :ttl 0 :quit t)
                      ("^\\*Alchemist-IEx\\*$" :size 0.25 :vslot -4 :select t :quit nil :ttl nil)))

  (set-repl-handler! 'elixir-ts-mode #'alchemist-iex-project-run)

  (add-hook 'alchemist-iex-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)
              ;; alchemist is hard-coded to use company :(
              (corfu-mode -1)
              (company-mode 1)))

  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "c") "Compile")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "e") "Execute")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "f") "Info")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "h") "Help")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "i") "iEX")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "m") "Mix")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "mt") "Test")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "o") "Macro Expand")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "p") "Project")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "v") "Eval")
  (which-key-add-keymap-based-replacements alchemist-mode-map (concat alchemist-key-command-prefix "X") "Hex"))
