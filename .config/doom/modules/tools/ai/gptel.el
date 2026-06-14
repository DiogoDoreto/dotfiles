;;; gptel.el -*- lexical-binding: t; -*-

;;; Bootstrap

(load! "gptel-oneshot.el")

(declare-function +gptel-openrouter-refresh-model-metadata "gptel-openrouter")

;;; Package configuration

(use-package gptel-agent
  :defer t)

(use-package gptel
  :defer t
  :config
  (require 'gptel-agent)
  (require 'gptel-agent-tools-introspection)
  (gptel-agent-update)

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

  (load! "gptel-prompts.el")
  (load! "my-gptel-tools.el")

  (add-hook 'gptel-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)))

  (setq gptel-system-prompt (alist-get 'assistant gptel-directives)
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)

  (when (s-equals? "lapdog" (system-name))
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key #'gptel-api-key-from-auth-source
      :models '(moonshotai/kimi-k2.7-code
                minimax/minimax-m3))
    (load! "gptel-openrouter.el")
    (+gptel-openrouter-refresh-model-metadata)

    (setq gptel-model 'gemma-4-E4B
          gptel-backend (gptel-make-openai "LLM:Local"
                          :protocol "http"
                          :host "127.0.0.1:8080"
                          :endpoint "/v1/chat/completions"
                          :stream t
                          :models '(gemma-4-E4B)))))

;;; Model selection

(defun +gptel--read-backend-model ()
  "Read a gptel backend/model pair using `gptel--infix-provider' candidates."
  (unless gptel--known-backends
    (user-error "No gptel backends are configured"))
  (cl-loop
   for (name . backend) in gptel--known-backends
   nconc (cl-loop for model in (gptel-backend-models backend)
                  collect (list (concat name ":" (gptel--model-name model))
                                backend model))
   into models-alist
   with completion-extra-properties =
   `(:annotation-function
     ,(lambda (comp)
        (let* ((model (nth 2 (assoc comp models-alist)))
               (desc (get model :description))
               (caps (get model :capabilities))
               (context (get model :context-window))
               (input-cost (get model :input-cost))
               (output-cost (get model :output-cost))
               (cutoff (get model :cutoff-date)))
          (when (or desc caps context input-cost output-cost cutoff)
            (concat
             (propertize " " 'display `(space :align-to 40))
             (when desc (truncate-string-to-width desc 70 nil ? t t))
             " " (propertize " " 'display `(space :align-to 112))
             (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
             " " (propertize " " 'display `(space :align-to 134))
             (when context (format "%5dk" context))
             " " (propertize " " 'display `(space :align-to 142))
             (when input-cost (format "$%5.2f in" input-cost))
             (if (and input-cost output-cost) "," " ")
             " " (propertize " " 'display `(space :align-to 153))
             (when output-cost (format "$%6.2f out" output-cost))
             " " (propertize " " 'display `(space :align-to 166))
             cutoff)))))
   finally return
   (cdr (assoc (completing-read
                "Model: " models-alist nil t nil nil
                (when (and gptel-backend gptel-model)
                  (concat (gptel-backend-name gptel-backend) ":"
                          (gptel--model-name gptel-model))))
               models-alist))))

(defun +gptel-set-model (&optional global)
  "Set `gptel-backend' and `gptel-model' without opening `gptel-menu'.

In a gptel chat buffer, set them buffer-locally.  With prefix argument
GLOBAL, set the global defaults instead."
  (interactive "P")
  (pcase-let ((`(,backend ,model) (+gptel--read-backend-model)))
    (if (and gptel-mode (not global))
        (setq-local gptel-backend backend
                    gptel-model model)
      (setq-default gptel-backend backend
                    gptel-model model))
    (message "gptel model set %s to %s:%s"
             (if (and gptel-mode (not global)) "locally" "globally")
             (gptel-backend-name backend)
             (gptel--model-name model))))
