;;; $DOOMDIR/dd/magit-delta.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Use delta to highlight Magit diffs, but let some files opt out of it.
;;
;; Upstream `magit-delta' pipes the whole raw diff of a section through delta in
;; a single call, then colorizes the entire result with overlays.  Both steps
;; are linear in the size of the diff, so a single generated file (a lockfile,
;; a minified bundle) is enough to make `magit-status' or `magit-diff' crawl.
;;
;; The code below splits the raw diff back into its per-file chunks and only
;; feeds delta the ones that are neither matched by
;; `dd/magit-delta-exclude-regexps' nor bigger than `dd/magit-delta-max-bytes'.
;; Adjacent kept files are merged into one region, so the common case is still a
;; single delta call.  Excluded files fall back to Magit's own rendering, with
;; their +/- markers left visible.

(defvar dd/magit-delta-exclude-regexps
  '("\\(?:\\`\\|/\\)pnpm-lock\\.yaml\\'"
    "\\(?:\\`\\|/\\)package-lock\\.json\\'"
    "\\(?:\\`\\|/\\)yarn\\.lock\\'"
    "\\(?:\\`\\|/\\)flake\\.lock\\'"
    "\\(?:\\`\\|/\\)Cargo\\.lock\\'"
    "\\(?:\\`\\|/\\)go\\.sum\\'"
    "\\.min\\.\\(?:js\\|css\\)\\'")
  "Regexps of file names whose diffs bypass delta.
Matched against the repository-relative file name, unanchored.")

(defvar dd/magit-delta-max-bytes 200000
  "Bypass delta for any single file diff larger than this many bytes.
Set to nil to only rely on `dd/magit-delta-exclude-regexps'.")

(defun dd/magit-delta-exclude-p (file bytes)
  "Return non-nil when the BYTES long diff of FILE should bypass delta."
  (or (and dd/magit-delta-max-bytes (> bytes dd/magit-delta-max-bytes))
      (and file (seq-some (lambda (re) (string-match-p re file))
                          dd/magit-delta-exclude-regexps))))

(defun dd/magit-delta--file-name (beg end)
  "Return the file name of the raw diff between BEG and END, or nil.
Only the first few lines are inspected, as that is where git puts the
headers."
  (save-excursion
    (let ((limit (min end (progn (goto-char beg) (line-end-position 8)))))
      (cl-flet ((header (re)
                  (goto-char beg)
                  (and (re-search-forward re limit t) (match-string 1))))
        ;; Prefer the "+++" name, so that renames are reported under their new
        ;; name; deletions fall back to "---", as their "+++" is /dev/null.
        (or (header "^\\+\\+\\+ b/\\(.+\\)$")
            (header "^--- a/\\(.+\\)$")
            (header "^diff --git a/\\(.+\\) b/")
            (header "^diff --\\(?:cc\\|combined\\) \\(.+\\)$"))))))

(defun dd/magit-delta--file-diffs ()
  "Return the per-file chunks of the raw diff in the current buffer.
Each element is (BEG END FILE), in buffer order.  Anything preceding the
first file header (a diffstat, for instance) is not covered."
  (save-excursion
    (goto-char (point-min))
    (let (starts)
      (while (re-search-forward "^diff " nil t)
        (push (match-beginning 0) starts))
      ;; STARTS is in reverse buffer order, so each entry ends where the
      ;; previously visited one begins.
      (let ((end (point-max)) diffs)
        (dolist (beg starts)
          (push (list beg end (dd/magit-delta--file-name beg end)) diffs)
          (setq end beg))
        diffs))))

(defun dd/magit-delta--regions ()
  "Return the regions of the raw diff that delta should handle.
Each element is (BEG . END), covering one or more adjacent file diffs.  The
list is in reverse buffer order, so that rewriting a region cannot
invalidate the positions of the regions left to process."
  (let (regions)
    (pcase-dolist (`(,beg ,end ,file) (dd/magit-delta--file-diffs))
      (unless (dd/magit-delta-exclude-p file (- end beg))
        (if (and regions (= (cdar regions) beg))
            (setcdr (car regions) end)
          (push (cons beg end) regions))))
    regions))

(defun dd/magit-delta--filter (text args)
  "Return TEXT piped through delta, called with ARGS."
  (with-temp-buffer
    (insert text)
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
      (apply #'call-process-region (point-min) (point-max)
             magit-delta-delta-executable t t nil args))
    (buffer-string)))

(defun dd/magit-delta-call-delta-on-included-files ()
  "Replacement for `magit-delta-call-delta-and-convert-ansi-escape-sequences'.
Behaves the same, except that file diffs rejected by
`dd/magit-delta-exclude-p' are left as Magit produced them."
  (let ((buffer-read-only nil)
        (args (magit-delta--make-delta-args)))
    (save-excursion
      (pcase-dolist (`(,beg . ,end) (dd/magit-delta--regions))
        (let ((text (delete-and-extract-region beg end)))
          (goto-char beg)
          (insert (dd/magit-delta--filter text args)))
        (save-restriction
          (narrow-to-region beg (point))
          (xterm-color-colorize-buffer 'use-overlays)
          (when magit-delta-hide-plus-minus-markers
            (magit-delta-hide-plus-minus-markers)))))))

(use-package magit-delta
  :defer t
  :hook magit-mode
  :config
  (advice-add #'magit-delta-call-delta-and-convert-ansi-escape-sequences
              :override #'dd/magit-delta-call-delta-on-included-files))
