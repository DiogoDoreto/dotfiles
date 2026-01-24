;;; $DOOMDIR/dd/dired.el -*- lexical-binding: t; -*-

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

(defvar dd--http-server-process nil
  "Variable to store the current HTTP server process.")

(defun dd/toggle-http-server ()
  "Start a Python HTTP server in Dired, or stop it if it's already running."
  (interactive)
  (if (process-live-p dd--http-server-process)
      (progn
        (delete-process dd--http-server-process)
        (setq dd--http-server-process nil)
        (message "HTTP Server stopped."))
    (let ((default-directory (cond
                              ((derived-mode-p 'dired-mode) (dired-current-directory))
                              ((fboundp 'projectile-project-root) (projectile-project-root))
                              ((and (fboundp 'project-current) (project-current))
                               (expand-file-name (project-root (project-current))))
                              (t default-directory))))
      (setq dd--http-server-process
            (start-process "http-server" "*http-server-output*" "nix" "shell" "nixpkgs#python3" "--command" "python3" "-m" "http.server"))
      (when (yes-or-no-p "Open browser?")
        (browse-url "http://localhost:8000"))
      (message "HTTP Server started for %s at http://localhost:8000" default-directory))))

(map! :leader :desc "HTTP server" "t h" #'dd/toggle-http-server)
