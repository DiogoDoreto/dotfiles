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
                 :size (if (string= "lapdog" (system-name)) 24 17)))

(defun dd--night? ()
  "Return t if current time is after 8pm or before 8am, else nil."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 20) (< hour 8))))

(setq modus-themes-to-toggle (if (string= "lapdog" (system-name))
                                 '(ef-dark modus-operandi)
                               '(ef-maris-dark ef-day)))

(map! :leader :desc "Theme" :n "t t" #'modus-themes-toggle)

(setq doom-theme (if (or (string= "lapdog" (system-name))
                         (dd--night?))
                     (car modus-themes-to-toggle)
                   (cadr modus-themes-to-toggle)))

(setq modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-bold-constructs nil
      modus-themes-italic-constructs t
      modus-themes-completions '((t . (bold)))
      modus-themes-prompts '(bold)
      modus-themes-headings
      '((agenda-structure . (light 2.0))
        (agenda-date . (regular 1.3))
        (0 . (light 2.0))
        (1 . (light 1.4))
        (2 . (light 1.3))
        (t . (regular 1.15))))

(when (string= "lapdog" (system-name))
  (setq ef-dark-palette-overrides
        '((bg-main "#000000")))) ; Make use of that deep OLED black

(after! tree-sitter
  (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)
  (set-face-attribute 'tree-sitter-hl-face:comment nil :slant 'italic))

(remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
(setq doom-modeline-lsp-icon nil
      doom-modeline-modal nil
      doom-modeline-major-mode-icon t
      doom-modeline-vcs-max-length 20
      doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq display-line-numbers-type 'relative)

;;; Flymake / Flycheck diagnostics

(setq flymake-show-diagnostics-at-end-of-line t)

(after! flycheck
  (map! (:map flycheck-mode-map
         :n "[e" #'flycheck-previous-error
         :n "]e" #'flycheck-next-error)
        (:leader
         :desc "Explain error"  :n "c ," #'flycheck-explain-error-at-point
         :desc "List errors"    :n "c x" #'flycheck-list-errors)))

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

(setq org-directory (if (string= "lapdog" (system-name))
                        "~/Nextcloud/Notes/"
                      "~/org/"))

(setq org-roam-directory (expand-file-name "roam/" org-directory))

(map! :map evil-org-mode-map :n "z g" #'+org/play-gif-at-point)

(defun dd/scan-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$")))

(dd/scan-org-agenda-files)

(map! :leader :desc "Scan org agenda files" "nU" #'dd/scan-org-agenda-files)

(add-hook 'org-mode-hook 'auto-fill-mode)

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

;; When something changes a file, automatically refresh the buffer containing that file so they can’t get out of sync.
(global-auto-revert-mode t)

;;; Package management

(defun dd/diff-bump-package-at-point (&optional select)
  "Show a diff of the available update to the package recipe under the cursor.

Based on the code of `doom/bump-package-at-point'"
  (interactive "P")
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist _beg _end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                                              (straight-vc-git--encode-url upstream-repo upstream-host)))
           (id (or (when url
                     (message "Checking last commit...")
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

(load! "dd/common-lisp")
(load! "dd/dired")
(load! "dd/elisp")
(load! "dd/javascript")
(load! "dd/lsp")
(load! "dd/nix")
(load! "dd/terminal")

(pcase (system-name)
  ("dogdot" (load! "dd/host-dogdot"))
  ("lapdog" (load! "dd/host-lapdog"))
  ("DT-5RHWB24" (load! "dd/dunst")))

;;; General mappings

;; navigate through visual lines by default
(map! :nv "j" #'evil-next-visual-line
      :nv "k" #'evil-previous-visual-line)

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

(defun dd/elfeed-search-sync ()
  "Update feeds and re-sync unread status.
Based on https://github.com/fasheng/elfeed-protocol/issues/28"
  (interactive)
  (mark-whole-buffer)
  (cl-loop for entry in (elfeed-search-selected)
           do (elfeed-untag-1 entry 'unread)) ; local operation, won't sync
  (elfeed-search-update--force)
  (let ((host (cadr (string-split (car (elfeed-protocol-feed-list)) "+"))))
    (elfeed-protocol-fever-reinit host)))

(map! :map elfeed-search-mode-map
      :desc "Toggle read tag" "<tab>" (cmd! (elfeed-search-toggle-all 'unread)))

(map! :localleader :map elfeed-search-mode-map
      :desc "Update feeds"    "m" #'dd/elfeed-search-sync
      :desc "Mark read"       "r" #'elfeed-search-untag-all-unread
      :desc "Mark unread"     "u" #'elfeed-search-tag-all-unread
      :desc "Open in browser" "o" #'elfeed-search-browse-url)

(map! :localleader :map elfeed-show-mode-map
      :desc "Mark read"   "r" (cmd! (elfeed-show-untag 'unread))
      :desc "Mark unread" "u" (cmd! (elfeed-show-tag 'unread)))

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

(use-package! ef-themes
  :after modus-themes
  :init
  (modus-themes-include-derivatives-mode 1))

(use-package! tts
  :defer t
  :commands (tts-read tts-mode tts-kokoro-start-server))

(use-package! magit-delta
  :defer t
  :hook magit-mode)

(use-package! info-rename-buffer
  :defer t
  :hook 'Info-selection-hook)

;; TODO fix jinx compilation when toggling jinx-mode
;; (use-package! jinx)
;; (add-hook 'emacs-startup-hook #'global-jinx-mode)

(use-package! ct :defer t)

(use-package! groovy-mode :defer t)

(use-package! devdocs-browser
  :defer t
  :config
  (map! :leader "s k" #'devdocs-browser-open-in))

(use-package! writegood-mode)

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

(use-package! elfeed-protocol
  :after elfeed
  :config
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-protocol-enabled-protocols '(fever))
  (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag t)
  (setq elfeed-protocol-feeds '(("fever+https://admin@freshrss.local.doreto.com.br"
                                 :api-url "https://freshrss.local.doreto.com.br/api/fever.php"
                                 :password "freshrss")))
  (elfeed-protocol-enable))

(use-package! combobulate
  :hook prog-mode)

;;; Random stuff...

(setq +doom-dashboard-menu-sections
      `(,@(seq-take +doom-dashboard-menu-sections 5)
        ("Read Feeds"
         :icon (nerd-icons-mdicon "nf-md-rss_box" :face 'doom-dashboard-menu-title)
         :action elfeed)
        ("Read HackerNews"
         :icon (nerd-icons-faicon "nf-fa-hacker_news" :face 'doom-dashboard-menu-title)
         :action hnreader-news)))

;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
(defun dd/screenshot-emacs ()
  "Save a screenshot of the current frame as a PNG image by default.
With a prefix argument (C-u), save as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((use-svg (when current-prefix-arg t))
         (extension (if use-svg ".svg" ".png"))
         (format (if use-svg 'svg 'png))
         (filename (make-temp-file "Emacs" nil extension))
         (data (x-export-frames nil format)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;; remove when fixed: https://github.com/doomemacs/doomemacs/issues/8585
(advice-add #'+nix/lookup-option :before
            (lambda (&rest _args)
              (require 'nixos-options)))
