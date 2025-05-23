;;; my-gptel-tools.el --- My collection of gptel tools -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'gptel)

(gptel-make-tool
 :function (lambda (filepath)
             (with-temp-buffer
               (insert-file-contents (expand-file-name filepath))
               (buffer-string)))
 :name "read_file"
 :description "Read and display the contents of a file"
 :args (list '(:name "filepath"
               :type string
               :description "Path to the file to read. Supports relative paths and ~."))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (directory)
             (mapconcat #'identity
                        (directory-files directory)
                        "\n"))
 :name "list_directory"
 :description "List the contents of a given directory"
 :args (list '(:name "directory"
               :type string
               :description "The path to the directory to list"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (expand-file-name name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :args (list '(:name "parent"
               :type string
               :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
               :type string
               :description "The name of the new directory to create, e.g. testdir"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (path filename content)
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :name "create_file"
 :description "Create a new file with the specified content"
 :args (list '(:name "path"
               :type string
               :description "The directory where to create the file")
             '(:name "filename"
               :type string
               :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file"))
 :category "filesystem")

(defun my-gptel-tools--edit_file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (if (and file-path (not (string= file-path "")) file-edits)
      (with-current-buffer (get-buffer-create "*edit-file*")
        (insert-file-contents (expand-file-name file-path))
        (let ((inhibit-read-only t)
              (case-fold-search nil)
              (file-name (expand-file-name file-path))
              (edit-success nil))
          ;; apply changes
          (dolist (file-edit (seq-into file-edits 'list))
            (when-let ((line-number (plist-get file-edit :line_number))
                       (old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string))
                       (is-valid-old-string (not (string= old-string ""))))
              (goto-char (point-min))
              (forward-line (1- line-number))
              (when (search-forward old-string nil t)
                (replace-match new-string t t)
                (setq edit-success t))))
          ;; return result to gptel
          (if edit-success
              (progn
                ;; show diffs
                (ediff-buffers (find-file-noselect file-name) (current-buffer))
                (format "Successfully edited %s" file-name))
            (format "Failed to edited %s" file-name))))
    (format "Failed to edited %s" file-path)))

(gptel-make-tool
 :function #'my-gptel-tools--edit_file
 :name "edit_file"
 :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
 :args (list '(:name "file-path"
               :type string
               :description "The full path of the file to edit")
             '(:name "file-edits"
               :type array
               :items (:type object
                       :properties
                       (:line_number
                        (:type integer :description "The line number of the file where edit starts.")
                        :old_string
                        (:type string :description "The old-string to be replaced.")
                        :new_string
                        (:type string :description "The new-string to replace old-string.")))
               :description "The list of edits to apply on the file"))
 :category "filesystem")

(gptel-make-tool
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live." buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(gptel-make-tool
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :name "append_to_buffer"
 :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to append text to.")
             '(:name "text"
               :type string
               :description "The text to append to the buffer."))
 :category "emacs")

(defun my-gptel-tools--codel-edit-buffer (buffer-name old-string new-string)
  "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))  ;; Case-sensitive search
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format "Error: Could not find text to replace in buffer %s" buffer-name)
            (if (> count 1)
                (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (format "Successfully edited buffer %s" buffer-name))))))))

(gptel-make-tool
 :name "EditBuffer"
 :function #'my-gptel-tools--codel-edit-buffer
 :description "Edits Emacs buffers"
 :args '((:name "buffer_name"
          :type string
          :description "Name of the buffer to modify")
         (:name "old_string"
          :type string
          :description "Text to replace (must match exactly)")
         (:name "new_string"
          :type string
          :description "Text to replace old_string with"))
 :category "edit")

(defun my-gptel-tools--codel-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

(gptel-make-tool
 :name "ReplaceBuffer"
 :function #'my-gptel-tools--codel-replace-buffer
 :description "Completely overwrites buffer contents"
 :args '((:name "buffer_name"
          :type string
          :description "Name of the buffer to overwrite")
         (:name "content"
          :type string
          :description "Content to write to the buffer"))
 :category "edit")

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
