;;; dd/embark.el -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Add Embark actions for inserting paths to buffer and file targets.
;; Relative paths use the invoking file's directory, or `default-directory'
;; when the invoking buffer is not visiting a file.

(defun dd/embark-path (path absolute)
  "Format PATH relative to the invoking buffer or as ABSOLUTE."
  (let* ((base-directory (or (and buffer-file-name
                                  (file-name-directory buffer-file-name))
                             default-directory))
         (absolute-path (expand-file-name path base-directory)))
    (if absolute
        (abbreviate-file-name absolute-path)
      (let ((relative-path (file-relative-name absolute-path base-directory)))
        (if (string-prefix-p "../" relative-path)
            relative-path
          (concat "./" relative-path))))))

(defun dd/embark-buffer-path (buffer absolute)
  "Return BUFFER's file path, or its name when it has no file.
When ABSOLUTE is non-nil, abbreviate the absolute path."
  (if-let* ((path (buffer-local-value 'buffer-file-name (get-buffer buffer))))
      (dd/embark-path path absolute)
    buffer))

(defun dd/embark-insert-buffer-relative-path (buffer)
  "Insert BUFFER's file path relative to the invoking buffer."
  (interactive "bBuffer: ")
  (insert (dd/embark-buffer-path buffer nil)))

(defun dd/embark-insert-buffer-absolute-path (buffer)
  "Insert BUFFER's abbreviated absolute file path."
  (interactive "bBuffer: ")
  (insert (dd/embark-buffer-path buffer t)))

(defun dd/embark-copy-buffer-relative-path (buffer)
  "Copy BUFFER's file path relative to the invoking buffer to the kill ring."
  (interactive "bBuffer: ")
  (let ((path (dd/embark-buffer-path buffer nil)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun dd/embark-copy-buffer-absolute-path (buffer)
  "Copy BUFFER's abbreviated absolute file path to the kill ring."
  (interactive "bBuffer: ")
  (let ((path (dd/embark-buffer-path buffer t)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun dd/embark-insert-file-relative-path (file)
  "Insert FILE relative to the invoking buffer."
  (interactive "FFile: ")
  (insert (dd/embark-path file nil)))

(defun dd/embark-insert-file-absolute-path (file)
  "Insert FILE's abbreviated absolute path."
  (interactive "FFile: ")
  (insert (dd/embark-path file t)))

(defun dd/embark-copy-file-relative-path (file)
  "Copy FILE relative to the invoking buffer to the kill ring."
  (interactive "FFile: ")
  (let ((path (dd/embark-path file nil)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun dd/embark-copy-file-absolute-path (file)
  "Copy FILE's abbreviated absolute path to the kill ring."
  (interactive "FFile: ")
  (let ((path (dd/embark-path file t)))
    (kill-new path)
    (message "Copied: %s" path)))

(after! embark
  (map! :map embark-buffer-map
        "i" #'dd/embark-insert-buffer-relative-path
        "I" #'dd/embark-insert-buffer-absolute-path
        "y" #'dd/embark-copy-buffer-relative-path
        "Y" #'dd/embark-copy-buffer-absolute-path
        :map embark-file-map
        "i" #'dd/embark-insert-file-relative-path
        "I" #'dd/embark-insert-file-absolute-path
        "y" #'dd/embark-copy-file-relative-path
        "Y" #'dd/embark-copy-file-absolute-path))

(provide 'dd-embark)
