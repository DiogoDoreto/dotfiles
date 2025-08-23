;;; $DOOMDIR/config-javascript.el -*- lexical-binding: t; -*-

(use-package! dd-taskrunner-javascript
  :config
  (map! :map npm-mode-command-keymap "r" #'dd-taskrunner-javascript))

(setq lsp-eslint-run "onSave")

(add-hook! (+format-with-lsp-mode)
  (when (and (eq major-mode 'typescript-mode) (bound-and-true-p +format-with-lsp-mode))
    (+format-with-lsp-mode -1)))

(after! apheleia
  (add-to-list 'apheleia-formatters '(xo "apheleia-npx" "xo" "--fix" "--stdin")))

(after! projectile
  (add-to-list 'projectile-other-file-alist '("ts" . ("spec.ts")))
  (add-to-list 'projectile-other-file-alist '("spec.ts" . ("ts"))))

(after! org
  (require 'ob-js)
  (defalias 'org-babel-execute:javascript #'org-babel-execute:js)

  ;; adapted from `org-babel-execute:js'
  (defun org-babel-execute:typescript (body params)
    "Execute TypeScript BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
    (let* ((org-babel-js-cmd (or (cdr (assq :cmd params)) org-babel-js-cmd))
           (result-type (cdr (assq :result-type params)))
           (full-body (org-babel-expand-body:generic
                       body params (org-babel-variable-assignments:js params)))
           (script-file (org-babel-temp-file "ts-script-" ".ts"))
           (result (progn
                     (with-temp-file script-file
                       (insert
                        ;; return the value or the output
                        (if (string= result-type "value")
                            (format org-babel-js-function-wrapper full-body)
                          full-body)))
                     (org-babel-eval
                      (format "%s %s" org-babel-js-cmd
                              (org-babel-process-file-name script-file)) ""))))
      (org-babel-result-cond (cdr (assq :result-params params))
        result (org-babel-js-read result))))

  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  (add-to-list 'org-babel-tangle-lang-exts '("javascript" . "js"))
  (add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts")))

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

(after! compile
  ;; eslint errors parsing for compilation-mode
  ;; from https://github.com/Fuco1/compile-eslint/blob/master/compile-eslint.el

  (defun compile-eslint--find-filename ()
    "Find the filename for current error."
    (save-match-data
      (save-excursion
        (when (re-search-backward (rx bol (group "/" (+ any)) eol))
          (list (match-string 1))))))

  (let ((form `(eslint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-eslint--find-filename
                2 3 2 1)))
    (if (assq 'eslint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))

  (push 'eslint compilation-error-regexp-alist))
