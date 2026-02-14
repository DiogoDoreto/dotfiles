;;; $DOOMDIR/dd/terminal.el --- terminal customizations -*- lexical-binding: t; -*-

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

(defun dd/run-cmd (command directory buffer-name)
  "Run COMMMAND inside DIRECTORY and send output to BUFFER-NAME"
  (let ((default-directory directory))
    (compilation-start command nil (lambda (_mode) buffer-name))))

;;; Tramp

(setq tramp-use-connection-share t
      tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                              "-o ControlMaster=auto -o ControlPersist=yes"))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; VTerm

(setq vterm-shell "fish")

(defun dd/vterm (name)
  "Create a new vterm buffer that won't be removed automatically."
  (interactive "sVTerm buffer name: ")
  (let (display-buffer-alist)
    (vterm name)))

(map! :leader :desc "Open named vterm" :n "o v" #'dd/vterm)

(map! :map vterm-mode-map
      :ni "C-<escape>" #'vterm-send-escape
      :desc "paste" :ni "C-S-v" (cmd! (vterm-send-string (current-kill 0) t))
      :desc "shift-return" :ni "S-RET" (cmd! (vterm-send-key "\C-m" t))
      :desc "shift-return" :ni "S-<return>" (cmd! (vterm-send-key "\C-m" t))
      :desc "alt-return" :ni "M-RET" (cmd! (vterm-send-key "\C-m" nil t))
      :desc "alt-return" :ni "M-<return>" (cmd! (vterm-send-key "\C-m" nil t)))

;;; Compilation

(use-package fancy-compilation
  :commands fancy-compilation-mode
  :config
  (setq fancy-compilation-override-colors nil))

(with-eval-after-load 'compile
  (fancy-compilation-mode))
