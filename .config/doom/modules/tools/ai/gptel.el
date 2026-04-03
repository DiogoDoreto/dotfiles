;;; gptel.el -*- lexical-binding: t; -*-

(load! "mcp.el")
(load! "gptel-oneshot.el")

(use-package gptel
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

  (load! "gptel-prompts.el")
  (load! "my-gptel-tools.el")

  (when (s-equals? "lapdog" (system-name))
    (gptel-make-openai "Cerebras"
      :host "api.cerebras.ai"
      :endpoint "/v1/chat/completions"
      :stream nil
      :key (lambda ()
             (and-let* ((auth (car (auth-source-search :host "cerebras")))
                        (secret-fn (plist-get auth :secret)))
               (funcall secret-fn)))
      :models '(qwen-3-235b-a22b-thinking-2507
                qwen-3-coder-480b)))

  (setq gptel--system-message (alist-get 'assistant gptel-directives)
        gptel-model 'gpt-5-mini
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)

  (defun dd--gh-parse-enabled-models (api-response)
    "Return a list of models with policy state `enabled' from the API-RESPONSE."
    (let* ((data (cdr (assoc 'data api-response))))
      ;; (pp data)
      (delq nil
            (mapcar (lambda (model)
                      (let* ((policy (assoc 'policy model))
                             (state (when policy (cdr (assoc 'state (cdr policy)))))
                             (id (cdr (assoc 'id model)))
                             (model-picker-enabled (assoc 'model_picker_enabled model)))
                        (when (or model-picker-enabled
                                  (and state (string= state "enabled")))
                          id)))
                    data))))

  (defun dd/gh-request-enabled-models ()
    "Call Github's models API and print the model-ids that are currently enabled.
Also see multipliers here: https://docs.github.com/en/enterprise-cloud@latest/copilot/reference/ai-models/supported-models#model-multipliers"
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
                  (message "Github's enabled models: %S" (dd--gh-parse-enabled-models data)))))))
