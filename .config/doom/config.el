;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

(setq frame-title-format '((multiple-frames (:eval (+workspace-current-name)))
                           (multiple-frames " | ")
                           "%b"
                           (:eval (if (buffer-modified-p) " ×"))
                           " | Emacs "
                           emacs-version))

(set-frame-parameter nil 'alpha 98)
(add-to-list 'default-frame-alist '(alpha . 98))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "VictorMono Nerd Font Propo" :size 17 :weight 'semi-light))

(after! tree-sitter
  (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)
  (set-face-attribute 'tree-sitter-hl-face:comment nil :slant 'italic))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(setq doom-modeline-lsp-icon nil)
(setq doom-modeline-modal nil)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq lsp-enable-file-watchers nil)
(setq lsp-eslint-run "onSave")
(add-hook! (+format-with-lsp-mode)
  (when (and (eq major-mode 'typescript-mode) (bound-and-true-p +format-with-lsp-mode))
    (+format-with-lsp-mode -1)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode))

(setq org-directory "~/org/")

(defun dd/scan-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively org-directory "\.org$")))
(dd/scan-org-agenda-files)
(map! :leader :desc "Scan org agenda files" "nU" #'dd/scan-org-agenda-files)

(setq vterm-shell "fish")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! "C-s" 'save-buffer)

(map! :n "g b" 'browse-url)

(map! :n "M-<left>" #'evil-window-left)
(map! :n "M-<down>" #'evil-window-down)
(map! :n "M-<up>" #'evil-window-up)
(map! :n "M-<right>" #'evil-window-right)

(map! "C-<down>" #'drag-stuff-down)
(map! "C-<up>" #'drag-stuff-up)

(map! :i "C-<tab>" 'yas-expand)

;; something was overwriting this to cause indentation
(map! :n "C-i" 'better-jumper-jump-forward)

(map! :n "] e" 'flycheck-next-error)
(map! :n "[ e" 'flycheck-previous-error)

(map! :leader :n "c I" 'dd/js-install)
(map! :leader :n "c j" 'consult-lsp-file-symbols)
(map! :leader :n "c J" 'consult-lsp-symbols)
(map! :leader :n "c X" 'lsp-eslint-apply-all-fixes)

(map! :leader :n "w w" #'dd/window-prefix)

(map! :leader :desc "Format buffer with apheleia" :n "c F" 'apheleia-format-buffer)

(map! :map dired-mode-map :n "<backspace>" 'dired-up-directory)

(use-package! magit-delta
  :hook magit-mode)

(use-package! fancy-compilation
  :commands fancy-compilation-mode)
(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package! jest-test-mode
  :commands jest-test-mode
  :hook (typescript-mode typescript-tsx-mode))
(map! :localleader
      :mode (typescript-mode typescript-tsx-mode)
      (:prefix ("j" . "Jest")
       :desc "Rerun last test"       "j" #'jest-test-rerun-test
       :desc "Run current buffer"    "f" #'jest-test-run
       :desc "Run current project"   "p" #'jest-test-run-all-tests
       :desc "Run test under cursor" "t" #'jest-test-run-at-point))
