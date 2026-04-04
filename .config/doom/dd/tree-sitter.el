;;; tree-sitter.el -*- lexical-binding: t; -*-

(defun +tree-sitter-get-textobj (group &optional query)
  "A wrapper around `evil-textobj-tree-sitter-get-textobj' to
prevent eager expansion."
  (eval `(evil-textobj-tree-sitter-get-textobj ,group ,query)))

(defun +tree-sitter-goto-textobj (group &optional previous end query)
  "Thin wrapper that returns the symbol of a named function, used in keybindings."
  (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
    (fset sym (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj group previous end query)))
    sym))

(use-package evil-textobj-tree-sitter
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (after! tree-sitter (require 'evil-textobj-tree-sitter))
  :after-call doom-first-input-hook
  :config
  (defvar +tree-sitter-goto-previous-map (make-sparse-keymap))
  (defvar +tree-sitter-goto-next-map (make-sparse-keymap))

  (evil-define-key 'normal 'global
    "[g" +tree-sitter-goto-previous-map
    "]g" +tree-sitter-goto-next-map)

  (map! (:map evil-inner-text-objects-map
              "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
              "f" (+tree-sitter-get-textobj "function.inner")
              "F" (+tree-sitter-get-textobj "call.inner")
              "C" (+tree-sitter-get-textobj "class.inner")
              "v" (+tree-sitter-get-textobj "conditional.inner")
              "l" (+tree-sitter-get-textobj "loop.inner"))
        (:map evil-outer-text-objects-map
              "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
              "f" (+tree-sitter-get-textobj "function.outer")
              "F" (+tree-sitter-get-textobj "call.outer")
              "C" (+tree-sitter-get-textobj "class.outer")
              "c" (+tree-sitter-get-textobj "comment.outer")
              "v" (+tree-sitter-get-textobj "conditional.outer")
              "l" (+tree-sitter-get-textobj "loop.outer"))

        (:map +tree-sitter-goto-previous-map
              "a" (+tree-sitter-goto-textobj "parameter.outer" t)
              "f" (+tree-sitter-goto-textobj "function.outer" t)
              "F" (+tree-sitter-goto-textobj "call.outer" t)
              "C" (+tree-sitter-goto-textobj "class.outer" t)
              "c" (+tree-sitter-goto-textobj "comment.outer" t)
              "v" (+tree-sitter-goto-textobj "conditional.outer" t)
              "l" (+tree-sitter-goto-textobj "loop.outer" t))
        (:map +tree-sitter-goto-next-map
              "a" (+tree-sitter-goto-textobj "parameter.outer")
              "f" (+tree-sitter-goto-textobj "function.outer")
              "F" (+tree-sitter-goto-textobj "call.outer")
              "C" (+tree-sitter-goto-textobj "class.outer")
              "c" (+tree-sitter-goto-textobj "comment.outer")
              "v" (+tree-sitter-goto-textobj "conditional.outer")
              "l" (+tree-sitter-goto-textobj "loop.outer")))

  (after! which-key
    (setq which-key-allow-multiple-replacements t)
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1")))))
