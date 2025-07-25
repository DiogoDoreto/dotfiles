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
             (s-starts-with? "Remove import from " action-title)
             (s-starts-with? "Remove 'async'" action-title)
             (s-starts-with? "Remove unnecessary `await`" action-title))))

  (defun lsp-extra--run-code-action-with-title (action-title)
    "Retrive an up-to-date list of code-actions and run the one that matches the
ACTION-TITLE"
    (let ((selected-action (seq-find (lambda (hs) (string= (lsp:code-action-title hs) action-title))
                                     (lsp-code-actions-at-point))))
      (if selected-action (lsp-execute-code-action selected-action)
        (message "Could not find action: %s" action-title))))

  (defun lsp-extra-show-actions-at-point (ARG)
    "Display and optionally auto-execute LSP code actions at point.

This command collects available code actions at the current point. By default,
it will automatically execute a predefined action if one matches the criteria.
If no matching action is found, or if a universal argument (ARG) is provided, a
popup menu will list all available actions for selection."
    (interactive "P")
    (let* ((action-list (lsp-code-actions-at-point))
           (action-title-list (mapcar #'lsp-extra--code-action-to-popup-item action-list))
           (auto-exec-action-title (and (not ARG) (seq-find #'lsp-extra--should-auto-run-action action-title-list))))
      (cond
       (auto-exec-action-title
        (lsp-extra--run-code-action-with-title auto-exec-action-title))
       (action-title-list
        (lsp-extra--run-code-action-with-title (popup-menu* action-title-list :isearch t)))
       (t (message "No actions at point")))))

  (map! :leader
        :desc "LSP actions popup" :nv "c ." #'lsp-extra-show-actions-at-point
        :n "c j" 'consult-lsp-file-symbols
        :n "c J" 'consult-lsp-symbols
        :n "c X" 'lsp-eslint-apply-all-fixes))

(after! (lsp-mode vertico vertico-posframe)
  (add-to-list 'vertico-multiform-commands '(consult-lsp-file-symbols buffer (:not posframe)))
  (add-to-list 'vertico-multiform-commands '(consult-lsp-symbols buffer (:not posframe))))
