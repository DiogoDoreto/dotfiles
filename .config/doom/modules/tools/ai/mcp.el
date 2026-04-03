;;; mcp.el -*- lexical-binding: t; -*-

(use-package mcp
  :after gptel
  :config
  (setq mcp-hub-servers
        `(("playwright" . (:command "podman"
                           :args ,(string-split "run -i --rm --replace --name playwright-mcp --init --pull=always mcr.microsoft.com/playwright/mcp"))))))

(defun +mcp/prepare-filesystem (root)
  (interactive "DProject root directory: ")
  (let* ((root (expand-file-name root))
         (cmd (string-split (format "podman run -i --rm --replace --name filesystem-mcp --mount type=bind,src=%s,dst=%s docker.io/mcp/filesystem %s" root root root))))
    (mcp-stop-server "filesystem")
    (gptel-mcp-disconnect '("filesystem"))
    (setf (alist-get "filesystem" mcp-hub-servers nil t #'string=)
          `(:command ,(car cmd) :args ,(cdr cmd)))
    (gptel-mcp-connect '("filesystem") nil t)))
