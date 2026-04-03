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

  ;; -- Evil integration -----------------------------------------------------
  ;; ghostel renders into a read-only buffer and owns its keymap entirely, so
  ;; the approach mirrors evil-collection-vterm: always start in insert state,
  ;; reclaim the C- keys evil shadows in insert, and give normal state just
  ;; enough bindings to be useful before jumping back into the terminal.
  (after! evil
    ;; Always open ghostel buffers in insert state.
    (evil-set-initial-state 'ghostel-mode 'insert)

    (add-hook 'ghostel-mode-hook
              (lambda ()
                ;; Don't step the cursor back one character when leaving insert state --
                ;; cursor position inside a terminal is managed by the shell, not Emacs.
                (setq-local evil-move-cursor-back nil)
                (setq-local doom-real-buffer-p t)))

    ;; -- Insert state -------------------------------------------------------
    ;; Evil's insert-state-map shadows many C- keys (C-k inserts a digraph,
    ;; C-w deletes a word, C-u kills to indent, etc.).  Override them all so
    ;; keystrokes reach the shell instead.
    ;; ghostel--self-insert reads this-command-keys; control characters
    ;; (char < 128) are forwarded as their raw byte values (\x01, \x04, ...).
    ;; C-y is the exception: route through ghostel-yank so the shell's
    ;; bracketed-paste mode is respected.
    (evil-define-key 'insert ghostel-mode-map
      (kbd "C-a") #'ghostel--self-insert    ; beginning-of-line (readline)
      (kbd "C-d") #'ghostel--self-insert    ; delete-char / EOF
      (kbd "C-e") #'ghostel--self-insert    ; end-of-line (readline)
      (kbd "C-k") #'ghostel--self-insert    ; kill-to-eol (readline)
      (kbd "C-l") #'ghostel--self-insert    ; clear screen
      (kbd "C-n") #'ghostel--self-insert    ; next history
      (kbd "C-o") #'ghostel--self-insert    ; run command, keep line (readline)
      (kbd "C-p") #'ghostel--self-insert    ; previous history
      (kbd "C-r") #'ghostel--self-insert    ; reverse search
      (kbd "C-t") #'ghostel--self-insert    ; transpose chars (readline)
      (kbd "C-u") #'ghostel--self-insert    ; kill-to-bol (readline)
      (kbd "C-v") #'ghostel--self-insert    ; literal-next (readline)
      (kbd "C-w") #'ghostel--self-insert    ; delete-word-backward
      (kbd "C-y") #'ghostel-yank            ; yank with bracketed-paste support
      (kbd "C-z") #'ghostel--self-insert    ; suspend (SIGTSTP)
      (kbd "DEL")        (cmd! (ghostel--send-encoded "backspace" ""))  ; backspace
      (kbd "<backspace>") (cmd! (ghostel--send-encoded "backspace" ""))  ; backspace (GUI)
      ;; Send a literal escape to the terminal (distinct from ESC = exit insert).
      (kbd "C-<escape>") (cmd! (ghostel--send-key "\e"))
      ;; Paste from clipboard with bracketed-paste support.
      (kbd "C-S-v") (cmd! (ghostel--paste-text (current-kill 0))))

    ;; -- Normal state -------------------------------------------------------
    ;; The buffer is read-only and managed by the native module, so destructive
    ;; operators (d, c, s, ...) make no sense and are left as no-ops.
    (evil-define-key 'normal ghostel-mode-map
      ;; Return to insert state so keystrokes reach the shell again.
      "i" #'evil-insert
      "a" #'evil-append
      "I" #'evil-insert-line
      "A" #'evil-append-line
      ;; Prompt navigation via OSC 133 markers.
      (kbd "[[") #'ghostel-previous-prompt
      (kbd "]]") #'ghostel-next-prompt
      ;; Paste kill-ring entry into the terminal (with bracketed paste).
      "p" (cmd! (ghostel--paste-text (current-kill 0)))
      "P" (cmd! (ghostel--paste-text (current-kill 0)))
      ;; G: scroll to the live bottom rather than Emacs point-max.
      "G" (cmd! (when ghostel--term
                  (ghostel--scroll-bottom ghostel--term)
                  (ghostel--invalidate)))
      ;; v/V: enter ghostel's copy mode -- freezes the display and enables
      ;; standard Emacs navigation; C-SPC + move + M-w to copy, q to exit.
      "v" #'ghostel-copy-mode
      "V" #'ghostel-copy-mode
      ;; Send a literal escape to the terminal.
      (kbd "C-<escape>") (cmd! (ghostel--send-key "\e"))
      ;; Paste from clipboard with bracketed-paste support.
      (kbd "C-S-v") (cmd! (ghostel--paste-text (current-kill 0))))))

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

(map! :leader :desc "Open named ghostel" :n "o g" #'dd/ghostel)
