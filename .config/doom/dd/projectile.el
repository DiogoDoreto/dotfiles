;;; $DOOMDIR/dd/projectile.el --- Projectile tasks generation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Diogo Doreto

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

;;; Commentary:

;; Generates `projectile-tasks' for a project and persists them in its
;; .dir-locals.el.  Each generator below looks at the project root and
;; returns an alist of (TASK-NAME . COMMAND); `dd/projectile-generate-tasks'
;; concatenates the results.  Add new patterns by writing another generator
;; and registering it in `dd/projectile-task-generators'.

;;; Code:

(require 'files-x)
(require 'json)
(require 'projectile)

(defconst dd/projectile--node-lockfiles
  '(("pnpm-lock.yaml" . "pnpm")
    ("yarn.lock"      . "yarn")
    ("bun.lock"       . "bun")
    ("bun.lockb"      . "bun")
    ("package-lock.json" . "npm"))
  "Alist of (LOCKFILE . PACKAGE-MANAGER), in detection order.")

(defun dd/projectile--node-package-manager (root)
  "Return the package manager used by the project at ROOT.
Detected from the lockfile; defaults to npm."
  (or (cdr (seq-find (lambda (entry)
                       (file-exists-p (expand-file-name (car entry) root)))
                     dd/projectile--node-lockfiles))
      "npm"))

(defun dd/projectile--package-json-scripts (root)
  "Return the script names from ROOT/package.json, in file order."
  (let ((file (expand-file-name "package.json" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((json (json-parse-buffer :object-type 'alist))
             (scripts (alist-get 'scripts json)))
        (mapcar (lambda (entry) (symbol-name (car entry))) scripts)))))

(defun dd/projectile--node-tasks (root)
  "Return install and script tasks for the package.json at ROOT, if any."
  (when (file-exists-p (expand-file-name "package.json" root))
    (let ((pm (dd/projectile--node-package-manager root)))
      (cons (cons "install" (format "%s install" pm))
            (mapcar (lambda (script)
                      (cons (format "run %s" script)
                            (format "%s run %s" pm
                                    (if (string-match-p "\\`[[:alnum:]_:.@/-]+\\'" script)
                                        script
                                      (shell-quote-argument script)))))
                    (dd/projectile--package-json-scripts root))))))

(defun dd/projectile--nix-flake-tasks (root)
  "Return nix flake tasks if ROOT has a flake.nix."
  (when (file-exists-p (expand-file-name "flake.nix" root))
    '(("nix flake update" . "nix flake update"))))

;; A target line starts at column 0 with plain names followed by a single
;; colon (so `VAR := x' and pattern rules are not matched).
(defconst dd/projectile--makefile-target-regexp
  "^\\([[:alnum:]_][^:=$#%[:space:]]*\\(?:[ \t]+[[:alnum:]_][^:=$#%[:space:]]*\\)*\\)[ \t]*::?\\(?:$\\|[^=]\\)"
  "Regexp matching a Makefile rule line; group 1 holds the target names.")

(defun dd/projectile--makefile-targets (file)
  "Return the explicit, non-file-path target names declared in FILE."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward dd/projectile--makefile-target-regexp nil t)
        (dolist (target (split-string (match-string 1)))
          (unless (or (string-search "/" target)
                      (member target targets))
            (push target targets)))))
    (nreverse targets)))

(defun dd/projectile--makefile-tasks (root)
  "Return one `make TARGET' task per target of the Makefile at ROOT."
  (when-let* ((file (seq-find (lambda (name)
                                (file-exists-p (expand-file-name name root)))
                              '("GNUmakefile" "makefile" "Makefile"))))
    (mapcar (lambda (target)
              (cons (format "make %s" target) (format "make %s" target)))
            (dd/projectile--makefile-targets (expand-file-name file root)))))

(defvar dd/projectile-task-generators
  '(dd/projectile--node-tasks
    dd/projectile--makefile-tasks
    dd/projectile--nix-flake-tasks)
  "Functions taking the project root and returning an alist of tasks.")

(defun dd/projectile--saved-tasks (root)
  "Return the `projectile-tasks' saved in ROOT/.dir-locals.el, or nil."
  (let ((file (expand-file-name dir-locals-file root)))
    (when (file-exists-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (alist-get 'projectile-tasks (alist-get nil (read (current-buffer)))))
        (error nil)))))

(defun dd/projectile--write-tasks (root tasks)
  "Persist TASKS as `projectile-tasks' in ROOT/.dir-locals.el and apply them."
  (let* ((file (expand-file-name dir-locals-file root))
         (existing (find-buffer-visiting file))
         (inhibit-message t))
    (modify-dir-local-variable nil 'projectile-tasks tasks 'add-or-replace file)
    (with-current-buffer (find-buffer-visiting file)
      (save-buffer)
      (unless existing (kill-buffer))))
  ;; The current buffer already evaluated its dir-locals; re-apply them.
  (if buffer-file-name
      (progn (hack-dir-local-variables)
             (hack-local-variables-apply))
    (hack-dir-local-variables-non-file-buffer)))

;;;###autoload
(defun dd/projectile-generate-tasks ()
  "Generate projectile tasks for the current project into .dir-locals.el.
Non-destructive: tasks already saved keep their command when a generated
task has the same name, and only tasks not yet present are appended."
  (interactive)
  (let* ((root (projectile-acquire-root))
         (existing (dd/projectile--saved-tasks root))
         (generated (apply #'append
                           (mapcar (lambda (fn) (funcall fn root))
                                   dd/projectile-task-generators)))
         (added nil))
    (dolist (task generated)
      (unless (assoc (car task) existing)
        (setq existing (append existing (list task)))
        (push task added)))
    (cond
     (added
      (dd/projectile--write-tasks root existing)
      (message "Added %d projectile task%s in %s"
               (length added) (if (cdr added) "s" "")
               (abbreviate-file-name root)))
     (generated
      (message "No new tasks to add for %s" (projectile-project-name root)))
     (t
      (message "No tasks found for %s" (projectile-project-name root))))))

(defun dd/projectile--offer-task-generation (&rest _)
  "Offer to generate tasks when the current project has none.
Meant as :before advice on `projectile-run-task'."
  (when (and (projectile-project-root)
             (null (projectile-project-tasks))
             (y-or-n-p "No tasks for this project. Generate them? "))
    (dd/projectile-generate-tasks)))

(advice-add 'projectile-run-task :before #'dd/projectile--offer-task-generation)

(map! :leader
      :desc "Run project task" :n "p ," #'projectile-run-task)
