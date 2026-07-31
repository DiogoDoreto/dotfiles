;;; embark-buffer-path-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defvar embark-buffer-map (make-sparse-keymap))
(defvar embark-file-map (make-sparse-keymap))
(defvar embark-target-injection-hooks nil)
(defmacro after! (_package &rest body) `(progn ,@body))
(defmacro map! (&rest _args))

(load (expand-file-name "../dd/embark.el"
                        (file-name-directory load-file-name))
      nil t)

(ert-deftest dd-embark-inserts-buffer-path-relative-to-invoking-file ()
  (let ((target (generate-new-buffer "target")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name "/home/dog/projects/app/notes/current.md")
          (with-current-buffer target
            (setq buffer-file-name "/home/dog/projects/app/notes/src/main.el"))
          (dd/embark-insert-buffer-relative-path (buffer-name target))
          (should (equal (buffer-string) "./src/main.el")))
      (kill-buffer target))))

(ert-deftest dd-embark-preserves-parent-relative-buffer-path ()
  (let ((target (generate-new-buffer "target")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name "/home/dog/projects/app/notes/current.md")
          (with-current-buffer target
            (setq buffer-file-name "/home/dog/projects/shared/main.el"))
          (dd/embark-insert-buffer-relative-path (buffer-name target))
          (should (equal (buffer-string) "../../shared/main.el")))
      (kill-buffer target))))

(ert-deftest dd-embark-inserts-abbreviated-absolute-buffer-path ()
  (let ((target (generate-new-buffer "target")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer target
            (setq buffer-file-name (expand-file-name "projects/app/main.el" "~")))
          (dd/embark-insert-buffer-absolute-path (buffer-name target))
          (should (equal (buffer-string) "~/projects/app/main.el")))
      (kill-buffer target))))

(ert-deftest dd-embark-inserts-name-for-non-file-buffer ()
  (let ((target (generate-new-buffer "*pathless target*")))
    (unwind-protect
        (with-temp-buffer
          (dd/embark-insert-buffer-relative-path (buffer-name target))
          (insert "|")
          (dd/embark-insert-buffer-absolute-path (buffer-name target))
          (should (equal (buffer-string)
                         "*pathless target*|*pathless target*")))
      (kill-buffer target))))

(ert-deftest dd-embark-actions-use-injected-buffer-target ()
  (dolist (action '(dd/embark-insert-buffer-relative-path
                    dd/embark-insert-buffer-absolute-path))
    (let ((hooks (alist-get action embark-target-injection-hooks))
          allowed-edit)
      (with-temp-buffer
        (insert "selected buffer")
        (cl-letf (((symbol-function 'minibuffer-prompt-end) (lambda () 1))
                  ((symbol-function 'embark--ignore-target)
                   (lambda (&rest _)
                     (erase-buffer)
                     (setq allowed-edit t)))
                  ((symbol-function 'embark--allow-edit)
                   (lambda (&rest _) (setq allowed-edit t))))
          (dolist (hook hooks)
            (funcall hook :target "selected buffer"))
          (should (equal (buffer-string) "selected buffer"))
          (should-not allowed-edit))))))

(ert-deftest dd-embark-inserts-file-path-relative-to-invoking-file ()
  (with-temp-buffer
    (setq buffer-file-name "/home/dog/projects/app/notes/current.md")
    (dd/embark-insert-file-relative-path
     "/home/dog/projects/app/notes/src/main.el")
    (should (equal (buffer-string) "./src/main.el"))))

(ert-deftest dd-embark-inserts-abbreviated-absolute-file-path ()
  (with-temp-buffer
    (setq default-directory "/home/dog/projects/app/")
    (dd/embark-insert-file-absolute-path "../shared/main.el")
    (should (equal (buffer-string) "~/projects/shared/main.el"))))

(ert-deftest dd-embark-preserves-file-directory-trailing-slash ()
  (with-temp-buffer
    (setq buffer-file-name "/home/dog/projects/app/current.md")
    (dd/embark-insert-file-relative-path "/home/dog/projects/app/src/")
    (insert "|")
    (dd/embark-insert-file-absolute-path
     (expand-file-name "projects/app/src/" "~"))
    (should (equal (buffer-string)
                   "./src/|~/projects/app/src/"))))

(ert-deftest dd-embark-file-actions-use-injected-target ()
  (dolist (action '(dd/embark-insert-file-relative-path
                    dd/embark-insert-file-absolute-path))
    (should-not (alist-get action embark-target-injection-hooks))))

(provide 'embark-buffer-path-test)
