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

  (defun lsp-extra--should-auto-run-action (action-title)
    (and action-title
         (or (string= action-title "Add async modifier to containing function")
             (s-starts-with? "Remove unused declaration for:" action-title)
             (s-starts-with? "Remove import from " action-title))))

  (defun lsp-extra-show-actions-at-point ()
    (interactive)
    (let* ((action-list (lsp-code-actions-at-point))
           (action-title-list (mapcar #'lsp-extra--code-action-to-popup-item action-list)))
      (if (lsp-extra--should-auto-run-action (car action-title-list))
          (lsp-execute-code-action (car action-list))
        (if (not action-title-list) (message "No actions at point")
          (let* ((selected-action-title (popup-menu* action-title-list :isearch t))
                 (selected-action (seq-find (lambda (hs) (string= (lsp:code-action-title hs) selected-action-title))
                                            (lsp-code-actions-at-point))))
            (if selected-action (lsp-execute-code-action selected-action)
              (message "Could not find action: %s" selected-action-title)))))))


  (map! :leader
        :desc "LSP actions popup" :nv "c ." #'lsp-extra-show-actions-at-point
        :n "c j" 'consult-lsp-file-symbols
        :n "c J" 'consult-lsp-symbols
        :n "c X" 'lsp-eslint-apply-all-fixes))

(after! (lsp-mode vertico vertico-posframe)
  (add-to-list 'vertico-multiform-commands '(consult-lsp-file-symbols buffer (:not posframe)))
  (add-to-list 'vertico-multiform-commands '(consult-lsp-symbols buffer (:not posframe))))
