;;; gptel-oneshot.el --- One shot commands to send to gptel -*- lexical-binding: t; -*-

(defun dd--get-staged-diff (context-lines)
  "Return the diff of staged files with CONTEXT-LINES of context.
Lockfiles are ignored."
  (let ((default-directory (projectile-project-root)))
    (shell-command-to-string
     (concat (format "git diff --cached --unified=%d" context-lines)
             " -- . ':!package-lock.json' ':!yarn.lock' ':!flake.lock'"))))

;; ** Create commit command

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

;; ** Code review command

(defun dd--create-code-review-system-prompt ()
  (string-join
   '("You are an experienced senior software engineer performing a thorough code review. "
     "Your goal is to provide constructive, actionable feedback that improves code quality, maintainability, and security."
     ""
     "Focus on these key areas:"
     "1. **Code Quality**: Look for bugs, logic errors, edge cases, and potential runtime issues"
     "2. **Design & Architecture**: Evaluate if the changes follow good design principles (SOLID, DRY, etc.)"
     "3. **Performance**: Identify potential performance bottlenecks or inefficiencies"
     "4. **Security**: Check for security vulnerabilities, input validation, and data exposure risks"
     "5. **Readability**: Assess code clarity, naming conventions, and documentation"
     "6. **Testing**: Consider if changes need additional tests or if existing tests are affected"
     "7. **Best Practices**: Ensure adherence to language-specific and project conventions"
     ""
     "Structure your review as follows:"
     "- Start with a brief summary of the changes"
     "- List specific issues found (if any) with file references and code excerpt"
     "- Provide actionable suggestions for improvements"
     "- Highlight any positive aspects of the code"
     "- End with an overall assessment (Approve/Request Changes/Comment)"
     ""
     "Format the document using emacs' org-mode.")
   "\n"))

(defun dd--create-code-review-user-prompt (focus-area)
  (string-join
   `("<staged_changes>"
     ,(dd--get-staged-diff 20)
     "</staged_changes>"
     ,@(when focus-area (list "<review_focus>"
                              (concat "Please pay special attention to: " focus-area)
                              "</review_focus>"))
     "Please perform a comprehensive code review of the staged changes above."
     "The diff includes extended context (20 lines) around each change to help you understand the surrounding code.")
   "\n"))

(defun dd/gptel-code-review-staged-changes (ARG)
  "Run a code review of staged git changes using GPT via gptel.
With prefix ARG, prompt for a review focus."
  (interactive "P")
  (let ((focus-area (when ARG (read-string "Review focus: ")))
        (output-buffer (get-buffer-create (format "*Code Review<%s>*" (+workspace-current-name))))
        (gptel-use-context nil))
    (require 'gptel)
    (with-current-buffer output-buffer
      (erase-buffer)
      (org-mode)
      (display-buffer (current-buffer))
      (message "Preparing code review...")
      (gptel-request
          (dd--create-code-review-user-prompt focus-area)
        :system (dd--create-code-review-system-prompt)
        :stream t))))

(provide 'gptel-oneshot)
