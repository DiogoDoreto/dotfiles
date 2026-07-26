;;; nix-search-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defvar +lookup-provider-url-alist nil)
(defmacro map! (&rest _args))
(defmacro evil-local-set-key (&rest _args))

(let ((doomdir (expand-file-name ".." (file-name-directory load-file-name))))
  (load (expand-file-name "dd/nix.el" doomdir) nil t)
  (load (expand-file-name "dd/consult-omni-nix.el" doomdir) nil t))

(ert-deftest dd-nix-search-opens-package-record-details ()
  (let ((package '((package_attr_name . "ripgrep")
                   (package_pversion . "14.1.1")
                   (package_description . "Fast recursive search")
                   (package_programs . ("rg")))))
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (let ((buffer (dd-nix-search-show-package-details package)))
        (unwind-protect
            (with-current-buffer buffer
              (should (equal (buffer-name) "*Nix Package: ripgrep*"))
              (should (string-match-p "Nix Package details for: ripgrep"
                                      (buffer-string)))
              (should (string-match-p "package_programs: rg"
                                      (buffer-string))))
          (kill-buffer buffer))))))

(ert-deftest consult-omni-nix-selection-opens-attached-package-record ()
  (let* ((package '((package_attr_name . "ripgrep")
                    (package_pversion . "14.1.1")
                    (package_description . "Fast recursive search")
                    (package_programs . ("rg"))))
         (candidate (propertize "ripgrep  14.1.1  Fast recursive search  rg"
                               'dd-nix-package package))
         selected)
    (cl-letf (((symbol-function 'dd-nix-search-show-package-details)
               (lambda (record) (setq selected record))))
      (consult-omni--nix-open-package candidate)
      (should (equal selected package)))))

(ert-deftest consult-omni-nix-candidate-shows-package-summary ()
  (let* ((package '((package_attr_name . "ripgrep")
                    (package_pversion . "14.1.1")
                    (package_description . "Fast recursive search")
                    (package_programs . ("rg" "very-long-exported-program-name"))))
         (candidate (consult-omni--nix-format-candidate package "rip")))
    (should (string-match-p "ripgrep" candidate))
    (should (string-match-p "14.1.1" candidate))
    (should (string-match-p "Fast recursive search" candidate))
    (should (string-match-p "rg, very-long-exported-…" candidate))
    (should-not (string-match-p "very-long-exported-program-name" candidate))
    (should (equal (get-text-property 0 'dd-nix-package candidate) package))))

(ert-deftest consult-omni-nix-passes-query-as-one-opaque-argument ()
  (let (command callback)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'dd-nix-version-compare--run)
               (lambda (args done)
                 (setq command args callback done)
                 'search-process)))
      (consult-omni--nix-fetch-results "hello --count 10" :callback #'ignore)
      (should (equal command
                     '("nh" "search" "packages" "--json" "hello --count 10")))
      (should (functionp callback)))))

(ert-deftest consult-omni-nix-failure-clears-current-candidates ()
  (let (done result)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'dd-nix-version-compare--run)
               (lambda (_args callback)
                 (setq done callback)
                 'search-process)))
      (consult-omni--nix-fetch-results
       "ripgrep" :callback (lambda (candidates) (setq result candidates)))
      (funcall done 1 "backend unavailable")
      (should (null result)))))

(ert-deftest consult-omni-nix-ignores-obsolete-search-callbacks ()
  (let (callbacks cancelled results)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'delete-process)
               (lambda (process) (setq cancelled process)))
              ((symbol-function 'dd-nix-version-compare--run)
               (lambda (_args callback)
                 (push callback callbacks)
                 (intern (format "process-%d" (length callbacks))))))
      (consult-omni--nix-fetch-results
       "old" :callback (lambda (candidates) (push candidates results)))
      (consult-omni--nix-fetch-results
       "new" :callback (lambda (candidates) (push candidates results)))
      (should (eq cancelled 'process-1))
      (funcall (cadr callbacks) 1 "obsolete failure")
      (should (null results)))))

(ert-deftest consult-omni-nix-success-delivers-record-and-clears-process ()
  (let (done result)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'dd-nix-version-compare--run)
               (lambda (_args callback)
                 (setq done callback)
                 'search-process)))
      (consult-omni--nix-fetch-results
       "ripgrep" :callback (lambda (candidates) (setq result candidates)))
      (funcall done 0 "{\"results\":[{\"package_attr_name\":\"ripgrep\",\"package_pversion\":\"15.2.0\",\"package_programs\":[\"rg\"]}]}")
      (should (= (length result) 1))
      (should (equal (alist-get 'package_attr_name
                                (get-text-property 0 'dd-nix-package (car result)))
                     "ripgrep"))
      (should (null consult-omni--nix-process)))))

(ert-deftest consult-omni-nix-reports-missing-nh ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_executable) nil)))
    (should-error (consult-omni--nix-require-nh) :type 'user-error)))

(ert-deftest consult-omni-nix-defines-static-command-only ()
  (should (commandp 'consult-omni-nix-static))
  (should-not (fboundp 'consult-omni-nix)))

(provide 'nix-search-test)
