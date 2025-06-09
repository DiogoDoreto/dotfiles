;;; my-gptel-tools.el --- My collection of gptel tools -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'gptel)

(defcustom my-gptel-tools--allowed-directories '("/home/dog/projects/")
  "List of allowed directories where my-gptel-tools functions will be allowed to
  work on."
  :type '(repeat directory)
  :group 'my-gptel-tools)


(defun my-gptel-tools--expand-validate-path (filepath &optional default-dir)
  "Expand FILEPATH using DEFAULT-DIR if provided, and ensure the result is
inside one of the allowed directories.  If the expanded path is not allowed,
signal an error. Otherwise, return the expanded path."
  (let* ((fullpath (expand-file-name filepath default-dir))
         (allowed (cl-some (lambda (dir)
                             (string-prefix-p (expand-file-name dir default-dir) fullpath))
                           my-gptel-tools--allowed-directories)))
    (if allowed fullpath
      (error "File is not in an allowed directory!"))))

(gptel-make-tool
 :name "read_file"
 :description "Read and display the contents of a file"
 :category "filesystem"
 :function (lambda (filepath)
             (let ((fullpath (my-gptel-tools--expand-validate-path filepath)))
               (with-temp-buffer
                 (insert-file-contents fullpath)
                 (buffer-string))))
 :args (list '(:name "filepath"
               :type string
               :description "Path to the file to read. Supports relative paths and ~.")))

(gptel-make-tool
 :name "list_directory"
 :description "List the contents of a given directory"
 :category "filesystem"
 :function (lambda (directory)
             (let ((validated-dir (my-gptel-tools--expand-validate-path directory)))
               (mapconcat #'identity
                          (directory-files validated-dir)
                          "\n")))
 :args (list '(:name "directory"
               :type string
               :description "The path to the directory to list")))

(gptel-make-tool
 :name "make_directory"
 :description "Create a new directory with the given name in the specified parent directory"
 :category "filesystem"
 :function (lambda (parent name)
             (condition-case nil
                 (progn
                   (make-directory (my-gptel-tools--expand-validate-path name parent) t)
                   (format "Directory %s created/verified in %s" name parent))
               (error (format "Error creating directory %s in %s" name parent))))
 :args (list '(:name "parent"
               :type string
               :description "The parent directory where the new directory should be created, e.g. /tmp")
             '(:name "name"
               :type string
               :description "The name of the new directory to create, e.g. testdir")))

(gptel-make-tool
 :name "create_file"
 :description "Create a new file with the specified content"
 :category "filesystem"
 :function (lambda (path filename content)
             (let ((full-path (my-gptel-tools--expand-validate-path filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :args (list '(:name "path"
               :type string
               :description "The directory where to create the file")
             '(:name "filename"
               :type string
               :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file")))

(defun my-gptel-tools--edit_file (file-path file-edits)
  "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
  (let ((validated-path (my-gptel-tools--expand-validate-path file-path)))
    (if (and validated-path file-edits)
        (with-current-buffer (get-buffer-create "*my-gptel-tools--edit-file*")
          (insert-file-contents validated-path)
          (let ((inhibit-read-only t)
                (case-fold-search nil)
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
                  (ediff-buffers (find-file-noselect validated-path) (current-buffer))
                  (format "Successfully edited %s" validated-path))
              (format "Failed to edited %s" validated-path))))
      (format "Failed to edited %s" validated-path))))


(gptel-make-tool
 :name "edit_file"
 :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
 :category "filesystem"
 :function #'my-gptel-tools--edit_file
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
               :description "The list of edits to apply on the file")))

(gptel-make-tool
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :category "emacs"
 :function (lambda (buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "Error: buffer %s is not live." buffer))
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer whose contents are to be retrieved")))

(gptel-make-tool
 :name "append_to_buffer"
 :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
 :category "emacs"
 :function (lambda (buffer text)
             (with-current-buffer (get-buffer-create buffer)
               (save-excursion
                 (goto-char (point-max))
                 (insert text)))
             (format "Appended text to buffer %s" buffer))
 :args (list '(:name "buffer"
               :type string
               :description "The name of the buffer to append text to.")
             '(:name "text"
               :type string
               :description "The text to append to the buffer.")))

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
 :name "edit_buffer"
 :description "Edits a specific Emacs buffer"
 :category "emacs"
 :function #'my-gptel-tools--codel-edit-buffer
 :args '((:name "buffer_name"
          :type string
          :description "Name of the buffer to modify")
         (:name "old_string"
          :type string
          :description "Text to replace (must match exactly)")
         (:name "new_string"
          :type string
          :description "Text to replace old_string with")))

(defun my-gptel-tools--codel-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

(gptel-make-tool
 :name "replace_buffer"
 :description "Completely overwrites buffer contents"
 :category "emacs"
 :function #'my-gptel-tools--codel-replace-buffer
 :args '((:name "buffer_name"
          :type string
          :description "Name of the buffer to overwrite")
         (:name "content"
          :type string
          :description "Content to write to the buffer")))

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
