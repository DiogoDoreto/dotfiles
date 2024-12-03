(setq lsp-eslint-run "onSave")

(add-hook! (+format-with-lsp-mode)
  (when (and (eq major-mode 'typescript-mode) (bound-and-true-p +format-with-lsp-mode))
    (+format-with-lsp-mode -1)))

(after! projectile
  (message "after! projectile: add-to-list")
  ;; (customize-set-variable) or require it https://emacs.stackexchange.com/questions/32104/add-to-a-list-that-defcustom-has-not-yet-defined
  ;; maybe set :after in package recipe
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
