;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;; Author: Diogo Doreto
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(load! "whisper.el")
(load! "gptel-oneshot.el")

(use-package! mcp
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

(use-package! gptel
  :defer t
  :config
  (require 'gptel-integrations)
  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :side 'right
    :width 80
    :quit nil
    :ttl nil)

  (defun dd/gptel-context--use-file-name (orig-fun buffer contexts)
    "Advice to prefer file name over buffer name in the first line."
    (insert
     (with-temp-buffer
       (funcall orig-fun buffer contexts)
       (when-let* ((filename (buffer-file-name buffer)))
         (goto-char (point-min))
         (when (looking-at "In buffer `.*`:")
           (delete-region (point) (line-end-position))
           (insert (format "In file `%s`:" filename))))
       (buffer-string))))

  (advice-add 'gptel-context--insert-buffer-string
              :around #'dd/gptel-context--use-file-name)

  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))

  (load! "my-gptel-tools.el")

  (when (s-equals? "dogdot" (system-name))
    (gptel-make-ollama "Ollama"
      :host "chungus.home:11434"
      :stream t
      :models '(qwen2.5-coder:32b deepseek-r1:32b qwq)))

  (setq gptel-model 'gpt-4.1
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls t)

  (defun dd--gh-parse-enabled-models (api-response)
    "Return a list of models with policy state `enabled' from the API-RESPONSE."
    (let* ((data (cdr (assoc 'data api-response))))
      ;; (pp data)
      (delq nil
            (mapcar (lambda (model)
                      (let* ((policy (assoc 'policy model))
                             (state (when policy (cdr (assoc 'state (cdr policy)))))
                             (id (cdr (assoc 'id model))))
                        (when (and state (string= state "enabled"))
                          id)))
                    data))))

  (defun dd/gh-request-enabled-models ()
    "Call Github's models API and print the model-ids that are currently enabled"
    (interactive)
    (require 'request)
    (gptel--gh-auth)
    (request "https://api.githubcopilot.com/models"
      :sync t
      :type "GET"
      :headers `(("Authorization" . ,(concat "Bearer "
                                             (plist-get (gptel--gh-token gptel-backend) :token)))
                 ("copilot-integration-id" . "vscode-chat")
                 ("content-type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Github's enabled models: %S" (dd--gh-parse-enabled-models data))))))

  (setf (alist-get 'architect gptel-directives)
        "I want you to act as an IT Architect. I will provide some details about the functionality of an application or other digital product, and it will be your job to come up with ways to integrate it into the IT landscape. This could involve analyzing business requirements, performing a gap analysis and mapping the functionality of the new system to the existing IT landscape. Next steps are to create a solution design and, if needed, a physical network blueprint, definition of interfaces for system integration and a blueprint for the deployment environment.")

  (setf (alist-get 'sw-engineer gptel-directives)
        "You are a senior software engineer, programming expert, who provides precise answers, avoiding ambiguous responses.  Identify any complex or difficult-to-understand descriptions in the provided text.  Rewrite these descriptions to make them clearer and more accessible.  Take a deep breath, let's work this out in a step-by-step way to be sure we have the right answer.  When asked to implement a change, use the edit_buffer tool to perform the modifications in the relevant buffer.")

  (gptel-make-preset 'code
                     :description "Write code and modify buffers in context"
                     :system 'sw-engineer
                     :tools '("edit_buffer")
                     :confirm-tool-calls nil))


(use-package! aidermacs
  :defer t
  :config
  (setq aidermacs-backend 'vterm))

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-i" . 'copilot-accept-completion)
              ("M-i" . 'copilot-accept-completion-by-word)))

(map! :leader
      (:prefix ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort"             "x" #'gptel-abort
       :desc "Rewrite region" :v "r" #'gptel-rewrite
       :desc "Setup File System MCP" "f" #'+mcp/prepare-filesystem

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode

       :desc "model=gpt-4.1"          "1" (cmd! (setq gptel-model 'gpt-4.1))
       :desc "model=gpt-5"            "2" (cmd! (setq gptel-model 'gpt-5))
       :desc "model=gpt-4o"           "3" (cmd! (setq gptel-model 'gpt-4o))
       :desc "model=claude-sonnet-4"  "4" (cmd! (setq gptel-model 'claude-sonnet-4))
       :desc "model=grok-code-fast-1" "5" (cmd! (setq gptel-model 'grok-code-fast-1))

       :desc "Aider" "d" #'aidermacs-transient-menu

       :desc "Whisper Run"       "w" #'whisper-run
       :desc "Whisper File"      "W" #'whisper-file))

(map! :leader
      (:prefix ("l ." . "OneShot cmds")
       :desc "Create commit"         "c" #'dd/gptel-create-commit
       :desc "Review staged changes" "r" #'dd/gptel-code-review-staged-changes))

(defun whisper-command (input-file)
  "Produces whisper command to be run on the INPUT-FILE."
  (let ((model-file (expand-file-name (format "ggml-%s.bin" whisper-model)
                                      whisper-cpp-models-directory)))
    (unless (file-exists-p model-file)
      (if (yes-or-no-p (format "Model [%s] does not exist. Download to %s?" whisper-model whisper-cpp-models-directory))
          (progn (whisper-download-model)
                 (user-error "Model is downloading. Please try recording again when completed."))
        (user-error "Cannot transcribe. %s does not exist." model-file)))
    `(,(expand-file-name "bin/whisper-cli" whisper-cpp-directory)
      "--model" ,model-file
      ,input-file)))

(setq whisper-return-cursor-to-start nil)

(defun whisper-download-model ()
  "Download a Whisper model using the external whisper-cpp downloader.

Creates the directory `whisper-cpp-models-directory' if needed, then runs the
\"whisper-cpp-download-ggml-model\" binary from `whisper-cpp-directory' to
download the model specified by `whisper-model'."
  (interactive)
  (make-directory whisper-cpp-models-directory t)
  (let ((default-directory whisper-cpp-models-directory)
        (download-cmd (expand-file-name "bin/whisper-cpp-download-ggml-model" whisper-cpp-directory)))
    (compile (concat download-cmd " " whisper-model))))

(defun dd/clean-whisper-transcription ()
  "Remove Whisper timestamps and join buffer text into a single line."
  (goto-char (point-min))
  (let ((timestamp-regexp (rx line-start "[" (+ (any digit space ?- ":.>")) "]" (* space))))
    (while (re-search-forward timestamp-regexp nil t)
      (replace-match "")))
  (goto-char (point-min))
  (let ((linebreak-regexp (rx (* space) "\n" (* space))))
    (while (re-search-forward linebreak-regexp nil t)
      (replace-match " "))))

(add-hook 'whisper-after-transcription-hook #'dd/clean-whisper-transcription)

(load! "whisper-config.el")
