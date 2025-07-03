;;; gptel-oneshot.el --- One shot commands to send to gptel -*- lexical-binding: t; -*-

(defun dd--git-diff-staged ()
  "Return the diff of staged files only"
  (let ((default-directory (projectile-project-root)))
    (shell-command-to-string "git diff --cached -- . ':!package-lock.json' ':!yarn.lock' ':!flake.lock'")))

(defun dd--create-commit-system-prompt ()
  (concat "Your goal is to write good commit messages. "
          "You should follow the semantic commit conventions. "
          "Write a short and expressive first line. "
          "You are allowed to write more paragraphs, but only when the information in them add more value and do not repeat what's already stated on the first line."
          "Your answer will be used directly inside the git commit command, so do not respond with anything else besides the commit message itself. "
          "When a <commit_reason> block is provided, it must be used to write the message. "
          "Describing why a change was made is more important than describing what has changed. "))

(defun dd--create-commit-user-prompt (commit-reason)
  (concat "<current_git_diff>\n"
          (dd--git-diff-staged)
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
