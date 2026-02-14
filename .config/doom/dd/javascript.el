;;; $DOOMDIR/dd/javascript.el --- my customizations for js/ts projects -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Diogo Doreto

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Code:

;;; Global setup

(add-to-list 'auto-mode-alist '("\\.cts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))

(when (modulep! :tools lookup)
  (add-to-list '+lookup-provider-url-alist '("NPM" "https://www.npmjs.com/search?q=%s")))

;;; Use Packages

(use-package jsts :defer t)
(use-package jsts-package-json-mode :hook (json-ts-mode-hook . jsts-package-json--maybe-activate))

;;; After Packages

(with-eval-after-load 'apheleia
  (add-to-list 'apheleia-formatters '(eslint "apheleia-npx" "eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" file)))

(with-eval-after-load 'compile
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

(with-eval-after-load 'dape
  ;; node can natively load ts files now
  (push 'typescript-ts-mode (plist-get (alist-get 'js-debug-node dape-configs) 'modes))

  (let ((vitest-config (copy-sequence (alist-get 'js-debug-node dape-configs))))
    (plist-put! vitest-config
                :autoAttachChildProcesses t
                :program "node_modules/vitest/vitest.mjs"
                :args ["--test-timeout=0" "--no-file-parallelism" dape-buffer-default])
    (setf (alist-get 'js-debug-vitest dape-configs) vitest-config)))

(with-eval-after-load 'lsp-eslint
  (setq lsp-eslint-run "onSave"))

(with-eval-after-load 'nerd-icons
  (defun dd/ensure-extension-icon (copy-from exts)
    (let ((icon (cdr (assoc-string copy-from nerd-icons-extension-icon-alist))))
      (dolist (ext exts)
        (unless (assoc-string ext nerd-icons-extension-icon-alist)
          (push (cons ext icon) nerd-icons-extension-icon-alist)))))

  (dd/ensure-extension-icon "js" '("cjs" "mjs"))
  (dd/ensure-extension-icon "ts" '("cts" "mts")))

(with-eval-after-load 'org
  (require 'ob-js)

  ;; adapted from `org-babel-execute:js'
  (defun org-babel-execute:ts (body params)
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

  (defalias 'org-babel-execute:javascript #'org-babel-execute:js)
  (defalias 'org-babel-execute:typescript #'org-babel-execute:ts)

  (org-babel-do-load-languages 'org-babel-load-languages
                               (append '((js . t)
                                         (ts . t))
                                       org-babel-load-languages))
  (setq org-babel-tangle-lang-exts (append org-babel-tangle-lang-exts
                                           '(("js" . "js")
                                             ("javascript" . "js")
                                             ("ts" . "ts")
                                             ("typescript" . "ts"))))
  (setq org-src-lang-modes (append org-src-lang-modes
                                   '(("js" . javascript)
                                     ("javascript" . javascript)
                                     ("ts" . typescript-ts)
                                     ("typescript" . typescript-ts)))))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-other-file-alist '("ts" . ("spec.ts")))
  (add-to-list 'projectile-other-file-alist '("spec.ts" . ("ts"))))
