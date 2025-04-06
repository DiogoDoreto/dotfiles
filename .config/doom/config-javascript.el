;;; $DOOMDIR/config-javascript.el -*- lexical-binding: t; -*-

(setq lsp-eslint-run "onSave")

(add-hook! (+format-with-lsp-mode)
  (when (and (eq major-mode 'typescript-mode) (bound-and-true-p +format-with-lsp-mode))
    (+format-with-lsp-mode -1)))

(after! projectile
  (add-to-list 'projectile-other-file-alist '("ts" . ("spec.ts")))
  (add-to-list 'projectile-other-file-alist '("spec.ts" . ("ts"))))

(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode typescript-tsx-mode)
  :config
  (setq jest-test-compilation-error-regexp-alist-alist
        '((jest "\\([[:alnum:]/._-]+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3))))

(map! :localleader
      :mode (typescript-mode typescript-tsx-mode)
      (:prefix ("j" . "Jest")
       :desc "Rerun last test"       "j" #'jest-test-rerun-test
       :desc "Run current buffer"    "f" #'jest-test-run
       :desc "Run current project"   "p" #'jest-test-run-all-tests
       :desc "Run test under cursor" "t" #'jest-test-run-at-point))

(add-to-list '+lookup-provider-url-alist '("NPM" "https://www.npmjs.com/search?q=%s"))
