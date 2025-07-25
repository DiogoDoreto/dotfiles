;;; dd-javascript.el --- my functions to deal with js/ts projects -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Code:

(require 'cl-lib)
(require 'nerd-icons)

(push '("mjs" nerd-icons-devicon "nf-dev-javascript" :face nerd-icons-yellow) nerd-icons-extension-icon-alist)
(push '("mts" nerd-icons-sucicon "nf-seti-typescript" :face nerd-icons-blue-alt) nerd-icons-extension-icon-alist)

(defvar dd/js-lockfile-to-pkgmgr '(("package-lock.json" . "npm")
                                   ("yarn.lock" . "yarn")
                                   ("pnpm-lock.yaml" . "pnpm")
                                   ("bun.lockb" . "bun")))

(defun dd--js-find-current-pkgmgr ()
  "Return the package manager for current project."
  (let* ((files (directory-files (cdr (project-current))))
         (search (lambda (lockfile) (member lockfile files))))
    (cdr (cl-assoc-if search dd/js-lockfile-to-pkgmgr))))

;;;###autoload
(defun dd/js-install ()
  "Install dependencies using the correct package manager."
  (interactive)
  (let ((pkgmgr (dd--js-find-current-pkgmgr)))
    (if pkgmgr
        (compile (concat pkgmgr " install"))
      (message "No lockfile found."))))

(provide 'dd-javascript)
;;; dd-javascript.el ends here
