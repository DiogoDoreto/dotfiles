;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Personal info

(setq user-full-name "Diogo Doreto"
      user-mail-address "diogo@doreto.com.br")

;;; User Interface

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

(setq doom-font (font-spec
                 :family "VictorMono Nerd Font Mono"
                 :weight 'light
                 :size (if (s-equals? "lapdog" (system-name)) 24 17)))

(defun dd--night? ()
  "Return t if current time is after 8pm or before 8am, else nil."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 20) (< hour 8))))

(setq doom-theme (if (s-equals? "lapdog" (system-name))
                     'modus-vivendi-deuteranopia
                   (if (dd--night?) 'ef-maris-dark 'ef-day)))

(setq ef-themes-to-toggle '(ef-maris-dark ef-day))
(map! :leader :desc "Theme" :n "t t" #'ef-themes-toggle)

(after! tree-sitter
  (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)
  (set-face-attribute 'tree-sitter-hl-face:comment nil :slant 'italic))

(remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
(setq doom-modeline-lsp-icon nil)
(setq doom-modeline-modal nil)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq display-line-numbers-type 'relative)

;;; Flymake / Flycheck diagnostics

(setq flymake-show-diagnostics-at-end-of-line t)

(after! flycheck
  (map! :leader
        :desc "Explain error"  :n "c ," #'flycheck-explain-error-at-point
        :desc "List errors"    :n "c x" #'flycheck-list-errors))

;;; Jumping / Repeating

(map! :m "C-i"       #'better-jumper-jump-forward ;; something was overwriting this to cause indentation
      :m "<mouse-8>" #'better-jumper-jump-backward
      :m "<mouse-9>" #'better-jumper-jump-forward)

;; - The commands that should set a jump mark can be adviced with `doom-set-jump-maybe-a'
;; - The commands that should not be repeated with "." can be adviced with `evil-without-repeat-a'

(defun evil-without-repeat-a (fn &rest args)
  (evil-without-repeat
    (apply fn args)))

(advice-add #'+default/search-buffer :around #'doom-set-jump-maybe-a)

(after! flycheck
  ;; `flycheck-previous-error' uses `flycheck-next-error' internaly, just one advice is needed
  (advice-add #'flycheck-next-error :around #'doom-set-jump-maybe-a)
  (advice-add #'flycheck-next-error :around #'evil-without-repeat-a))

(after! avy
  (advice-add #'avy-process :around #'doom-set-jump-maybe-a)
  (advice-add #'avy-process :around #'evil-without-repeat-a))

;;; Window navigation

(map! :n "M-<left>"  #'evil-window-left
      :n "M-<down>"  #'evil-window-down
      :n "M-<up>"    #'evil-window-up
      :n "M-<right>" #'evil-window-right)

(map! :map treemacs-mode-map
      "M-<left>"  #'evil-window-left
      "M-<down>"  #'evil-window-down
      "M-<up>"    #'evil-window-up
      "M-<right>" #'evil-window-right)

(map! :leader
      :n "w w" #'dd/window-prefix
      :n "w ." #'ace-select-window
      :n "w X" #'ace-swap-window)

(defun dd/window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported.
From: https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window"
  (interactive)
  (display-buffer-override-next-command
   (lambda (_buffer _alist)
     (let ((window (aw-select (propertize " ACE" 'face 'mode-line-highlight))))
       (cons window 'reuse)))
   nil "[ace-window]")
  (message "Using `ace-window' to display next command buffer..."))

(use-package! ace-window
  :autoload (aw-select)
  :defer t)

;;; Org-mode

(setq org-directory "~/org/")

(map! :map evil-org-mode-map :n "z g" #'+org/play-gif-at-point)

(defun dd/scan-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")))

(dd/scan-org-agenda-files)

(map! :leader :desc "Scan org agenda files" "nU" #'dd/scan-org-agenda-files)

(use-package! ol-eww :after org)

(use-package! org-block-capf
  :hook (org-mode-hook . org-block-capf-add-to-completion-at-point-functions))

;;; Projectile

(setq projectile-project-search-path '("~/projects"))

(map! :leader :desc "Find related file" :n "f o" #'projectile-find-other-file)

;;; Snippets

(map! :i "C-<tab>" #'yas-expand)

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

;;; File modes

(add-to-list 'auto-mode-alist '("\\.keymap\\'" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

;;; Package management

(defun dd/diff-bump-package-at-point (&optional select)
  "Show a diff of the available update to the package recipe under the cursor.

Based on the code of `doom/bump-package-at-point'"
  (interactive "P")
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist beg end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                  (straight-vc-git--encode-url upstream-repo upstream-host)))
           (id (or (when url
                     (cdr (doom-call-process
                           "git" "ls-remote" url
                           (unless select branch))))
                   (user-error "Couldn't find a recipe for %s" package)))
           (id (car (split-string
                     (if select
                         (completing-read "Commit: " (split-string id "\n" t))
                       id))))
           (local-repo (doom-package-recipe-repo package))
           (repo-dir (straight--repos-dir local-repo))
           (default-directory repo-dir))
      (when (and oldid
                 (plist-member plist :pin)
                 (equal oldid id))
        (user-error "%s: no update necessary" package))
      (message "Preparing diff...")
      (doom-call-process "git" "fetch" "origin")
      (magit-diff-range (concat "..." id))
      (other-window-prefix)
      (magit-log-other (list (concat "..." id))))))

(map! :leader :prefix ("C-SPC p" . "  package mgmt")
      :desc "Diff this package with next bump" "d" #'dd/diff-bump-package-at-point
      :desc "Bump this package" "b" #'doom/bump-package-at-point)

;;; Extra config files

(load! "dd/elisp")
(load! "dd/javascript")
(load! "dd/lsp")
(load! "dd/nix")
(load! "dd/terminal")

(pcase (system-name)
  ("dogdot" (load! "dd/host-dogdot"))
  ("lapdog" (load! "dd/host-lapdog")))

;;; General mappings

;; Sane saving.
(map! "C-s" #'save-buffer)

;; Sane pasting. Was `evil-quoted-insert' before
(map! :i "C-v" (cmd! (insert (current-kill 0))))
(map! :map evil-ex-search-keymap
      "C-v" (cmd! (insert (current-kill 0))))

;; use middle click to go-to-definition - the down event ensures we navigate to the target point
(map! :n "<down-mouse-2>" #'evil-mouse-drag-region
      :n "<mouse-2>" #'+lookup/definition)

(defun dd/insert-semicolon-and-return ()
  "Save the current position in the line, move to the end, insert a semicolon,
and return to the original position."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(map! :ni "C-b" #'dd/insert-semicolon-and-return)

(map! :n "g b" #'browse-url)

(map! :leader :prefix "h" ;; help
      :desc "Browse man pages" :n "w" #'woman
      :leader :prefix "i" ;; insert
      :desc "Nerd icon" :n "n" #'nerd-icons-insert
      :leader :prefix "t" ;; toggle
      :desc "Auto fill"            :n "C" #'auto-fill-mode
      :desc "Highline cursor line" :n "L" #'hl-line-mode)

(map! :map dired-mode-map :n "<backspace>" #'dired-up-directory)

;;; After packages

(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets nil
        +corfu-want-tab-prefer-navigating-snippets t
        +corfu-want-tab-prefer-navigating-org-tables t))

(after! ibuffer
  ;; Update the value set by doom's :emacs/ibuffer module to increase the width
  ;; of the name column
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

(after! envrc
  (advice-add 'shell-command-to-string :around #'envrc-propagate-environment))

(after! good-scroll
  (setq good-scroll-step 20))

(after! qml-mode
  (add-hook 'qml-mode-hook #'lsp-deferred))

;;; Use packages

(use-package! tts
  :defer t
  :commands (tts-read tts-mode tts-kokoro-start-server))

(use-package! magit-delta
  :defer t
  :hook magit-mode)

(use-package! info-rename-buffer
  :defer t
  :hook 'Info-selection-hook)

(use-package! ct :defer t)

(use-package! groovy-mode :defer t)

(use-package! devdocs-browser
  :defer t
  :config
  (map! :leader "s k" #'devdocs-browser-open-in))

(use-package! hnreader
  :defer t
  :config
  (defun +hnreader/update-buffer (buf)
    (with-current-buffer buf
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'kill-current-buffer)))
  (advice-add #'hnreader--print-frontpage :after
              (defun +hnreader/frontpage-advice (_dom buf _url)
                (+hnreader/update-buffer buf)))
  (advice-add #'hnreader--print-comments  :after
              (defun +hnreader/comments-advice (_dom _url)
                (+hnreader/update-buffer (hnreader--get-hn-comment-buffer))))
  (advice-add #'hnreader--get-reply :override (lambda (_) nil))
  (set-popup-rule!
    (lambda (bname _action)
      (string= bname hnreader--comment-buffer))
    :select t
    :side 'right
    :width 0.5
    :quit nil
    :ttl nil))

(add-to-list '+doom-dashboard-menu-sections
             '("Read HackerNews"
               :icon (nerd-icons-faicon "nf-fa-hacker_news" :face 'doom-dashboard-menu-title)
               :action hnreader-news)
             t)
