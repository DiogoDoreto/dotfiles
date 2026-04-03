;;; copilot.el -*- lexical-binding: t; -*-

(use-package copilot
  :defer t
  :bind (:map copilot-completion-map
              ("C-f" . #'copilot-accept-completion)
              ("M-f" . #'copilot-accept-completion-by-word)
              ("C-e" . #'copilot-accept-completion-by-line)
              ("M-n" . #'copilot-next-completion)
              ("M-p" . #'copilot-previous-completion)))

(defvar-local dd/copilot-completion-enable nil
  "Whether to automatic turn on copilot suggestions.")

(add-hook
 'hack-local-variables-hook
 (defun dd/maybe-enable-copilot ()
   "Enable `copilot-mode' if `dd/copilot-completion-enable' is set to t."
   (when dd/copilot-completion-enable
     (copilot-mode 1))))
