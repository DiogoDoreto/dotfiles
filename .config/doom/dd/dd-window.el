;;; dd-window.el --- Window management helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Window management helpers
;;
;;; Code:

(require 'ace-window)

;;;###autoload
(defun dd/window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported.
From: https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window"
  (interactive)
  (display-buffer-override-next-command (lambda (buffer _)
                                          (let (window type)
                                            (setq
                                             window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
                                             type 'reuse)
                                            (cons window type)))
                                        nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(provide 'dd-window)
;;; dd-window.el ends here
