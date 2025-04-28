;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;; Author: Diogo Doreto
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(load! "whisper.el")

(use-package! gptel
  :defer t
  :config
  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))
  (and-let* ((auth-item (auth-source-search :host "github.com"))
             (token (funcall (plist-get (car auth-item) :secret))))
    (gptel-make-openai "Free Copilot"
      :host "models.inference.ai.azure.com"
      :endpoint "/chat/completions?api-version=2024-12-01-preview"
      :stream t
      :key token
      :models '(gpt-4o o1 DeepSeek-R1)))

  (defun my-gptel--edit_file (file-path file-edits)
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

  (setq gptel-model 'gpt-4o
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-confirm-tool-calls t
        gptel-tools (list (gptel-make-tool
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
                          (gptel-make-tool
                           :function #'my-gptel--edit_file
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
                           :category "filesystem"))))

(use-package! aidermacs
  :defer t
  :config
  (setq aidermacs-backend 'vterm))

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-i" . 'copilot-accept-completion)
              ("M-i" . 'copilot-accept-completion-by-word)))

(map! :leader
      (:prefix-map ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort"             "x" #'gptel-abort

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode

       :desc "model=gpt-4o" "1" (cmd! (setq gptel-model 'gpt-4o))
       :desc "model=o1"     "2" (cmd! (setq gptel-model 'o1))
       :desc "model=claude-3.7-sonnet"         "4" (cmd! (setq gptel-model 'claude-3.7-sonnet))
       :desc "model=claude-3.7-sonnet-thought" "5" (cmd! (setq gptel-model 'claude-3.7-sonnet-thought))

       :desc "Aider" "d" #'aidermacs-transient-menu

       :desc "Whisper Run"       "w" #'whisper-run
       :desc "Whisper File"      "W" #'whisper-file))

(setq whisper-return-cursor-to-start nil)

(defun dd/fill-after-whisper ()
  "Auto wrap the long generated line from Whisper"
  (evil-fill (line-beginning-position) (line-end-position)))

(add-hook 'whisper-after-insert-hook #'dd/fill-after-whisper)
