;;; common-lisp.el -*- lexical-binding: t; -*-

(with-eval-after-load 'sly
  (setq sly-complete-symbol-function #'sly-flex-completions)

  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)))

  (setf (alist-get 'sly--external-completion completion-styles-alist)
        '(sly--external-tryc
          sly--external-allc
          "Ad-hoc \"external completion\" style  (SLY flavor)")))
