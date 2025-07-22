;;; gptel-oneshot.el --- One shot commands to send to gptel -*- lexical-binding: t; -*-

(defun dd--get-staged-diff (context-lines)
  "Return the diff of staged files with CONTEXT-LINES of context.
Lockfiles are ignored."
  (let ((default-directory (projectile-project-root)))
    (shell-command-to-string
     (concat (format "git diff --cached --unified=%d" context-lines)
             " -- . ':!package-lock.json' ':!yarn.lock' ':!flake.lock'"))))

(defun dd--create-commit-system-prompt ()
  (concat "You are an expert programmer writing a commit message. "
          "Follow the semantic commit conventions. "
          "The first line must be a short, expressive summary. "
          "Subsequent paragraphs are optional. Use them to explain the 'why' behind the change, not the 'what'. "
          "Do not repeat information from the summary. "
          "If a <commit_reason> is provided, you must use it to inform your message. "
          "Output only the raw commit message text, with no additional explanations or markdown formatting."))

(defun dd--create-commit-user-prompt (commit-reason)
  (concat "<current_git_diff>\n"
          (dd--get-staged-diff 3)
          "\n</current_git_diff>\n"
          (when commit-reason
            (concat "<commit_reason>\n" commit-reason "</commit_reason>\n"))
          "\nWrite the commit message for the diff above."))

(defun dd--create-commit-callback (commit-message info)
  (if (not commit-message)
      (message "dd/gptel-create-commit failed with message: %s" (plist-get info :status))
    (require 'magit)
    (magit-commit-create `("--edit" "--message" ,commit-message))))

(defun dd/gptel-create-commit (ARG)
  (interactive "P")
  (let ((commit-reason (when ARG (read-string "Commit reason: "))))
    (require 'gptel)
    (message "Preparing commit...")
    (gptel-request
        (dd--create-commit-user-prompt commit-reason)
      :system (dd--create-commit-system-prompt)
      :callback #'dd--create-commit-callback)))

(provide 'gptel-oneshot)
