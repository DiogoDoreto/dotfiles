;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;;; Commentary:
;;
;;  Extra LSP related functionality
;;
;;; Code:

(require 'popup)

(after! lsp-mode
  (setq lsp-enable-file-watchers nil)

  (defun lsp-extra--code-action-to-popup-item (hs)
    (popup-make-item (lsp:code-action-title hs)))

  (defun lsp-extra-show-actions-at-point ()
    (interactive)
    (let* ((poplist (mapcar #'lsp-extra--code-action-to-popup-item (lsp-code-actions-at-point)))
           (action-title (popup-menu* poplist :isearch t))
           (action (seq-find (lambda (hs) (string= (lsp:code-action-title hs) action-title))
                             (lsp-code-actions-at-point))))
      (if action (lsp-execute-code-action action)
        (message "Could not find action: %s" action-title))))

  (map! :leader
        :desc "LSP actions popup" :nv "c ." #'lsp-extra-show-actions-at-point
        :n "c j" 'consult-lsp-file-symbols
        :n "c J" 'consult-lsp-symbols
        :n "c X" 'lsp-eslint-apply-all-fixes))

(after! (lsp-mode vertico vertico-posframe)
  (add-to-list 'vertico-multiform-commands '(consult-lsp-file-symbols buffer (:not posframe)))
  (add-to-list 'vertico-multiform-commands '(consult-lsp-symbols buffer (:not posframe))))
