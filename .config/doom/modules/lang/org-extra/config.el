;;; lang/org-extra/config.el -*- lexical-binding: t; -*-

(setq org-directory (if (string= "lapdog" (system-name))
                        "~/Nextcloud/Notes/"
                      "~/org/")
      dd/org-daily-directory (expand-file-name "daily/" org-directory))

(map! :map evil-org-mode-map :n "z g" #'+org/play-gif-at-point)

(map! :localleader :map org-mode-map
      :ni "a m" #'yank-media) ; Paste image from clipboard

(use-package ol-eww :after org)

(use-package org-block-capf
  :hook (org-mode-hook . org-block-capf-add-to-completion-at-point-functions))

(use-package org-mem
  :config
  (setq org-mem-do-look-everywhere nil
        org-mem-watch-dirs (list org-directory))

  (defun dd--scan-org-agenda-files (&rest _)
    (setq org-agenda-files
          (cl-loop
           for file in (org-mem-all-files)
           ;; Exclude *.org_archive or *archive/2025.org or similar
           unless (string-search "archive" file)
           when (seq-find (lambda (entry)
                            (or (org-mem-entry-active-timestamps entry)
                                (and (not (org-mem-entry-closed entry))
                                     (or (org-mem-entry-todo-state entry)
                                         (org-mem-entry-scheduled entry)
                                         (org-mem-entry-deadline entry)))))
                          (org-mem-entries-in file))
           collect file)))
  (add-hook 'org-mem-post-full-scan-functions #'dd--scan-org-agenda-files)

  (org-mem-updater-mode))

(use-package org-node
  :init
  (keymap-global-set "M-o" org-node-global-prefix-map)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-o" org-node-org-prefix-map))
  :config
  (setq org-node-property-crtime "TIME_CREATED"
        org-node-property-mtime "TIME_MODIFIED"
        org-node-display-sort-fn #'org-node-sort-by-mtime-property)

  (add-hook 'org-node-modification-hook #'org-node-update-mtime-property)

  (setq org-node-seq-defs
        (list
         (org-node-seq-def-on-any-sort-by-property
          "a" "All notes by last updated" org-node-property-mtime)

         `("d" :name "Daily/Journal"
           :version 2
           :classifier (lambda (node)
                         (when-let* ((daily-p (string-prefix-p (file-truename dd/org-daily-directory)
                                                               (org-mem-entry-file-truename node)))
                                     (path (org-mem-file node))
                                     (ymd (org-node-seq-filename->ymd path)))
                           (cons ymd path)))
           :whereami (lambda ()
                       (org-node-seq-filename->ymd buffer-file-name))
           :prompter (lambda (key)
                       (let ((org-node-seq-that-marks-calendar key))
                         (org-read-date)))
           :try-goto (lambda (item)
                       (org-node-seq-try-goto-file (cdr item)))
           :creator (lambda (sortstr key)
                      (let ((org-node-file-timestamp-format "")
                            (org-node-file-slug-fn (lambda (&rest _) sortstr))
                            (org-node-file-directory-ask dd/org-daily-directory))
                        (org-node-create (format-time-string
                                          "%Y-%b-%d %A"
                                          (org-time-string-to-time sortstr))
                                         (org-id-new)
                                         key))))))

  (org-node-cache-mode))
