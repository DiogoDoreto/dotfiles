;;; dd/consult-omni-nix.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult-omni source for Nix package search through nh.

;;; Code:

(require 'consult-omni)

(defvar consult-omni--nix-process nil
  "Current Nix package search process.")

(defvar consult-omni--nix-request 0
  "Generation number of the latest Nix package search request.")

(defun consult-omni--nix-error-message (output)
  "Return a concise error message from process OUTPUT."
  (truncate-string-to-width
   (or (car (split-string output "\n" t)) "unknown error") 160 nil nil t))

(defun consult-omni--nix-format-candidate (package query)
  "Format Nix PACKAGE record as a Consult candidate for QUERY."
  (let* ((source "Nix")
         (title (truncate-string-to-width
                 (or (alist-get 'package_attr_name package) "") 28 nil nil t))
         (version (truncate-string-to-width
                   (or (alist-get 'package_pversion package) "") 12 nil nil t))
         (description (truncate-string-to-width
                       (or (alist-get 'package_description package) "") 50 nil nil t))
         (programs (truncate-string-to-width
                    (string-join (or (alist-get 'package_programs package) '()) ", ")
                    24 nil nil t))
         (candidate (format "%-28s %-12s %-50s %s"
                            title version description programs)))
    (propertize candidate
                'dd-nix-package package
                :source source
                :title title
                :query query)))

(cl-defun consult-omni--nix-fetch-results
    (input &rest args &key callback &allow-other-keys)
  "Fetch Nix package results for INPUT and call CALLBACK."
  (ignore args)
  (when (process-live-p consult-omni--nix-process)
    (delete-process consult-omni--nix-process))
  (let ((request (cl-incf consult-omni--nix-request))
        process)
    (setq process
          (dd-nix-version-compare--run
           (dd-nix-search--command input)
           (lambda (exit-code output)
             (when (= request consult-omni--nix-request)
               (setq consult-omni--nix-process nil)
               (if (not (zerop exit-code))
                   (progn
                     (funcall callback nil)
                     (message "Nix package search failed: %s"
                              (consult-omni--nix-error-message output)))
                 (condition-case error-data
                     (funcall callback
                              (mapcar (lambda (package)
                                        (consult-omni--nix-format-candidate package input))
                                      (dd-nix-search--parse-output output)))
                   (error
                    (funcall callback nil)
                    (message "Nix package search failed: %s"
                              (consult-omni--nix-error-message
                               (error-message-string error-data))))))))))
    (setq consult-omni--nix-process process)))

(defun consult-omni--nix-open-package (candidate)
  "Open details for the Nix package attached to CANDIDATE."
  (when-let* ((package (get-text-property 0 'dd-nix-package candidate)))
    (dd-nix-search-show-package-details package)))

(defun consult-omni--nix-require-nh (&rest _args)
  "Report a clear error when the nh executable is unavailable."
  (unless (executable-find "nh")
    (user-error "Nix package search requires the nh executable")))

(consult-omni-define-source "Nix"
                            :narrow-char ?n
                            :type 'dynamic
                            :min-input 2
                            :require-match t
                            :category 'nix-package
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--nix-fetch-results
                            :on-return #'identity
                            :on-callback #'consult-omni--nix-open-package
                            :preview-key nil
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive 'dynamic
                            :enabled (lambda () (executable-find "nh"))
                            :annotate nil)

(advice-add 'consult-omni-nix-static :before #'consult-omni--nix-require-nh)

(provide 'consult-omni-nix)
