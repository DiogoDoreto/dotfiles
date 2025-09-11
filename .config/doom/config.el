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
(setq doom-font (font-spec
                 :family "VictorMono Nerd Font Mono"
                 :weight 'light
                 :size (if (s-equals? "lapdog" (system-name)) 24 17)))

(after! tree-sitter
  (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)
  (set-face-attribute 'tree-sitter-hl-face:comment nil :slant 'italic))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(defun dd--night? ()
  "Return t if current time is after 8pm or before 8am, else nil."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 20) (< hour 8))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme (if (s-equals? "lapdog" (system-name))
                     'modus-vivendi-deuteranopia
                   (if (dd--night?) 'ef-maris-dark 'ef-day)))

(setq ef-themes-to-toggle '(ef-maris-dark ef-day))
(map! :leader :desc "Theme" :n "t t" #'ef-themes-toggle)

(setq doom-modeline-lsp-icon nil)
(setq doom-modeline-modal nil)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))

(setq projectile-project-search-path (list "~/projects"))

(setq org-directory "~/org/")

(defun dd/scan-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")))
(dd/scan-org-agenda-files)
(map! :leader :desc "Scan org agenda files" "nU" #'dd/scan-org-agenda-files)

(setq vterm-shell "fish")

(setq flymake-show-diagnostics-at-end-of-line t)

(setq tramp-use-connection-share t
      tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                              "-o ControlMaster=auto -o ControlPersist=yes"))

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

(defun dd/insert-semicolon-and-return ()
  "Save the current position in the line, move to the end, insert a semicolon,
and return to the original position."
  (interactive)
  (let ((current-pos (point)))
    (end-of-line)
    (insert ";")
    (goto-char current-pos)))

(map! :ni "C-b" #'dd/insert-semicolon-and-return)

(map! :n "g b" 'browse-url)

(map! :n "M-<left>"  #'evil-window-left
      :n "M-<down>"  #'evil-window-down
      :n "M-<up>"    #'evil-window-up
      :n "M-<right>" #'evil-window-right)
(map! :map treemacs-mode-map
      "M-<left>"  #'evil-window-left
      "M-<down>"  #'evil-window-down
      "M-<up>"    #'evil-window-up
      "M-<right>" #'evil-window-right)

(map! :i "C-<tab>" 'yas-expand)

;; Sane pasting. Was `evil-quoted-insert' before
(map! :i "C-v" (cmd! (insert (current-kill 0))))
(map! :map evil-ex-search-keymap
      "C-v" (cmd! (insert (current-kill 0))))

(map! :m "C-i"       #'better-jumper-jump-forward ;; something was overwriting this to cause indentation
      :m "<mouse-8>" #'better-jumper-jump-backward
      :m "<mouse-9>" #'better-jumper-jump-forward)

;; use middle click to go-to-definition - the down event ensures we navigate to the target point
(map! :n "<down-mouse-2>" #'evil-mouse-drag-region
      :n "<mouse-2>" #'+lookup/definition)

(map! :leader :desc "Browse man pages" :n "h w" #'woman)

(map! :leader :desc "Format buffer with apheleia" :n "c F" 'apheleia-format-buffer)
(map! :leader :n "c I" 'dd/js-install)

(map! :leader :desc "Find related file" :n "f o" 'projectile-find-other-file)

(map! :leader
      :desc "Auto fill"            :n "t C" #'auto-fill-mode
      :desc "Highline cursor line" :n "t L" #'hl-line-mode)

(map! :leader
      :n "w w" #'dd/window-prefix
      :n "w ." #'ace-select-window
      :n "w X" #'ace-swap-window)

(map! :map dired-mode-map :n "<backspace>" 'dired-up-directory)

(map! :map vterm-mode-map :ni "C-<escape>" #'vterm-send-escape)

(map! :map evil-org-mode-map :n "z g" #'+org/play-gif-at-point)

(map! :localleader
      :map (emacs-lisp-mode-map lisp-interaction-mode-map)
      (:prefix ("d" . "debug")
               "t" #'trace-function
               "T" #'trace-function-background
               "u" #'untrace-function
               "U" #'untrace-all))

(advice-add #'+default/search-buffer :around #'doom-set-jump-maybe-a)

(after! flycheck
  (advice-add #'flycheck-next-error :around #'doom-set-jump-maybe-a)

  (map! :desc "Next error"     :m "]e" (cmd! (evil-without-repeat (flycheck-next-error)))
        :desc "Previous error" :m "[e" (cmd! (evil-without-repeat (flycheck-previous-error)))
        :leader
        :desc "Explain error"  :n "c ," #'flycheck-explain-error-at-point
        :desc "List errors"    :n "c x" #'flycheck-list-errors))

(map! :leader :prefix ("DEL" . "Mine!"))
(map! :leader :prefix ("DEL n" . " Nix"))

(when (s-equals? "dogdot" (system-name))
  (defun dd--make-nix (command)
    (let ((default-directory "~/projects/dotfiles/"))
      (compile command)))

  (map! :leader :prefix "DEL n"
        :desc "rebuild boot"        :n "b" (cmd! (dd--make-nix "make nixos-mini-boot"))
        :desc "rebuild switch"      :n "s" (cmd! (dd--make-nix "make nixos-mini-switch"))
        :desc "home-manager switch" :n "h" (cmd! (dd--make-nix "make hm-dog-mini-switch")))

  (map! :leader :prefix ("DEL s" . "SSH")
        :desc "Chungus dotfiles" :n "c" (cmd! (find-file "/sshx:chungus:p/dotfiles/"))))

(defun dd/vterm (name)
  "Create a new vterm buffer that won't be removed automatically."
  (interactive "sVTerm buffer name: ")
  (let (display-buffer-alist)
    (vterm name)))

(defun dd/run-cmd (command directory buffer-name)
  "Run COMMMAND inside DIRECTORY and send output to BUFFER-NAME"
  (let ((default-directory directory))
    (compilation-start command nil (lambda (_mode) buffer-name))))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets nil
        +corfu-want-tab-prefer-navigating-snippets t
        +corfu-want-tab-prefer-navigating-org-tables t))

(after! yasnippet-capf
  (remove-hook 'yas-minor-mode-hook #'+corfu-add-yasnippet-capf-h)
  (remove-hook 'completion-at-point-functions #'yasnippet-capf)
  (add-hook! 'yas-minor-mode-hook
    (defun dd/add-yasnippet-capf-h ()
      (add-hook 'completion-at-point-functions #'yasnippet-capf -10 t))))

(setq +file-templates-alist
      (append '(("/\\.envrc$"    :trigger "__envrc_use_flake" :mode envrc-file-mode)
                ("/\\flake.nix$" :trigger "__devShell-flake"  :mode nix-mode))
              +file-templates-alist))

(after! vertico-posframe
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center))

(after! ibuffer
  (setq ibuffer-formats
        `((mark modified read-only locked
           ,@(if (modulep! :emacs ibuffer +icons)
                 `(" " (icon 2 2 :left :elide)
                   ,(propertize " " 'display `(space :align-to 8)))
               '(" "))
           (name 40 40 :left)
           " " (size 9 -1 :right)
           " " (mode 16 16 :left :elide)
           ,@(when (require 'ibuffer-vc nil t)
               '(" " (vc-status 12 :left)))
           " " filename-and-process))))

(use-package! magit-delta
  :hook magit-mode)

(use-package! fancy-compilation
  :commands fancy-compilation-mode
  :config
  (setq fancy-compilation-override-colors nil))
(with-eval-after-load 'compile
  (fancy-compilation-mode))

(after! envrc
  (advice-add 'shell-command-to-string :around #'envrc-propagate-environment))

(use-package! info-rename-buffer :hook 'Info-selection-hook)

(use-package ct :defer t)

(use-package! cov
  :defer t
  :config
  (custom-set-faces
   '(cov-none-face  ((((class color)) :foreground "red")))
   '(cov-light-face ((((class color)) :foreground "orange")))
   '(cov-med-face   ((((class color)) :foreground "yellow")))
   '(cov-heavy-face ((((class color)) :foreground "green"))))
  (defun cov--locate-jest-lcov (file-dir file-name)
    (and-let* ((jest-root (projectile-locate-dominating-file file-dir "jest.config.js"))
               (lcov-file (f-join jest-root "test-results/lcov.info"))
               ((file-exists-p lcov-file)))
      (setq cov-lcov-project-root jest-root)
      (cons lcov-file 'lcov)))
  (add-to-list 'cov-coverage-file-paths #'cov--locate-jest-lcov))

(use-package! devdocs-browser
  :config
  (map! :leader "s k" #'devdocs-browser-open-in))

(use-package! org-block-capf
  :config
  (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions))

(load! "config-javascript")

(after! good-scroll
  (setq good-scroll-step 20))

(after! qml-mode
  (add-hook 'qml-mode-hook #'lsp))
