;;; $DOOMDIR/dd/pandoc.el -*- lexical-binding: t; -*-

(defvar dd/pandoc-formats
  '("commonmark" "gfm" "html" "json" "latex" "markdown" "mediawiki"
    "org" "rst" "textile")
  "Pandoc formats available for conversion.")

(defvar dd/pandoc-mode-format-alist
  '((markdown-mode . "markdown")
    (gfm-mode      . "gfm")
    (org-mode      . "org")
    (html-mode     . "html")
    (html-ts-mode  . "html")
    (latex-mode    . "latex")
    (LaTeX-mode    . "latex")
    (rst-mode      . "rst"))
  "Alist mapping major modes to pandoc format strings.")

(defvar dd/pandoc-format-mode-alist
  '(("markdown"   . markdown-mode)
    ("commonmark" . markdown-mode)
    ("gfm"        . markdown-mode)
    ("org"        . org-mode)
    ("html"       . html-mode)
    ("latex"      . latex-mode)
    ("rst"        . rst-mode))
  "Alist mapping pandoc format strings to major modes.")

(defun dd/pandoc--format-for-mode ()
  (or (alist-get major-mode dd/pandoc-mode-format-alist) "markdown"))

(defun dd/pandoc--mode-for-format (format)
  (or (alist-get format dd/pandoc-format-mode-alist nil nil #'string=)
      'fundamental-mode))

(defvar dd/pandoc-markdown-formats '("markdown" "commonmark" "gfm")
  "Input formats that need markdown-specific pre-processing.")

(defun dd/pandoc--preprocess-markdown ()
  "Insert a blank line before any heading not already preceded by one."
  (goto-char (point-min))
  (while (re-search-forward "\\([^\n]\\)\n\\(#+[ \t]\\)" nil t)
    (replace-match "\\1\n\n\\2")))

;;;###autoload
(defun dd/pandoc-convert ()
  "Convert the current region or buffer with pandoc via `nix run nixpkgs#pandoc'.
Prompts for source and target formats; output appears in a dedicated buffer."
  (interactive)
  (let* ((src-buf (current-buffer))
         (default-from (dd/pandoc--format-for-mode))
         (from (completing-read (format "From format [%s]: " default-from)
                                dd/pandoc-formats nil t nil nil default-from))
         (default-to (if (string= from "org") "gfm" "org"))
         (to   (completing-read "To format: " dd/pandoc-formats nil t nil nil default-to))
         (beg  (if (use-region-p) (region-beginning) (point-min)))
         (end  (if (use-region-p) (region-end)       (point-max)))
         (out-buf (get-buffer-create (format "*pandoc[%s→%s]*" from to)))
         (cmd (format "nix run nixpkgs#pandoc -- -f %s -t %s --wrap=preserve"
                      (shell-quote-argument from)
                      (shell-quote-argument to))))
    (with-current-buffer out-buf (erase-buffer))
    (if (member from dd/pandoc-markdown-formats)
        (with-temp-buffer
          (insert-buffer-substring src-buf beg end)
          (dd/pandoc--preprocess-markdown)
          (shell-command-on-region (point-min) (point-max) cmd out-buf nil "*pandoc-errors*"))
      (shell-command-on-region beg end cmd out-buf nil "*pandoc-errors*"))
    (with-current-buffer out-buf
      (when (string= to "org") ; remove property drawer with CUSTOM_ID
        (goto-char (point-min))
        (while (re-search-forward ":PROPERTIES:\n:CUSTOM_ID:[^\n]*\n:END:\n" nil t)
          (replace-match "")))
      (funcall (dd/pandoc--mode-for-format to)))
    (pop-to-buffer out-buf)))

(after! embark
  (add-to-list 'embark-target-injection-hooks '(dd/pandoc-convert embark--ignore-target))
  (define-key embark-region-map (kbd "P") #'dd/pandoc-convert)
  (define-key embark-buffer-map (kbd "P") #'dd/pandoc-convert))
