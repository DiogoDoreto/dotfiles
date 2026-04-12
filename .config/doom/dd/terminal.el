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
  (let ((display-buffer-alist nil)
        (buf-name (format "*vterm:%s:%s*"
                          (or (projectile-project-name) "-")
                          name)))
    (vterm buf-name)))

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

;;; Ghostel

(use-package ghostel
  :when (file-directory-p (expand-file-name
                           "straight/repos/ghostel"
                           (or (getenv "DOOMLOCALDIR")
                               (expand-file-name "~/.local/share/doomemacs"))))
  :defer t
  :config
  ;; ghostel-module.so lives alongside ghostel.el in the straight repo dir;
  ;; ghostel.el finds it automatically via (file-name-directory load-file-name).
  (setq ghostel-shell "fish"
        ;; Module is always provided by Nix — never prompt to download or compile.
        ghostel-module-auto-install nil)

  (add-hook 'ghostel-mode-hook (lambda () (setq-local doom-real-buffer-p t))))

;; -- Evil integration via evil-ghostel ------------------------------------
;; evil-ghostel-mode handles: initial insert state, evil-move-cursor-back,
;; C-a/d/e/k/n/p/r/t/u/w/y passthrough, p/P paste, i/a/I/A state entry,
;; d/c/s/x operators, cursor sync between normal↔insert, undo/redo.
(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  ;; -- Insert state: keys not in evil-ghostel's passthrough list ----------
  (evil-define-key 'insert evil-ghostel-mode-map
    (kbd "C-l") #'ghostel--self-insert    ; clear screen
    (kbd "C-o") #'ghostel--self-insert    ; run command, keep line (readline)
    (kbd "C-v") #'ghostel--self-insert    ; literal-next (readline)
    (kbd "C-z") #'ghostel--self-insert    ; suspend (SIGTSTP)
    (kbd "C-<escape>")  (cmd! (ghostel--send-key "\e"))
    (kbd "C-S-v")       (cmd! (ghostel--paste-text (current-kill 0))))

  ;; -- Normal state -------------------------------------------------------
  (evil-define-key 'normal evil-ghostel-mode-map
    ;; Prompt navigation via OSC 133 markers.
    (kbd "[[") #'ghostel-previous-prompt
    (kbd "]]") #'ghostel-next-prompt
    ;; G: scroll to the live bottom rather than Emacs point-max.
    "G" (cmd! (when ghostel--term
                (ghostel--scroll-bottom ghostel--term)
                (ghostel--invalidate)))
    (kbd "C-<escape>") (cmd! (ghostel--send-key "\e"))
    (kbd "C-S-v")      (cmd! (ghostel--paste-text (current-kill 0)))))

;; Disable corfu in ghostel mode (vterm is already excluded in Doom's corfu config).
(after! corfu
  (push 'ghostel-mode (cdar global-corfu-modes)))

(defun dd/ghostel (&optional arg)
  "Open ghostel with default directory at project root.
With universal ARG, use the directory of the current file instead."
  (interactive "P")
  (let ((default-directory
         (if arg
             (file-name-directory (or buffer-file-name default-directory))
           (or (doom-project-root) default-directory))))
    (call-interactively #'ghostel)))

(map! :leader :desc "Open ghostel term" :n "o g" #'dd/ghostel)
