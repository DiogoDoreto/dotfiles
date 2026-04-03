;;; agent-shell.el -*- lexical-binding: t; -*-

(use-package agent-shell
  :defer t
  :after acp
  :config
  (setq agent-shell-session-strategy 'prompt
        agent-shell-show-usage-at-turn-end t)
  (setq agent-shell-preferred-agent-config
        (if (string= (system-name) "DT-5RHWB24")
            (agent-shell-opencode-make-agent-config) ; or (agent-shell-github-make-copilot-config)
          (agent-shell-pi-make-agent-config))) ; or (agent-shell-anthropic-make-claude-code-config)))

  ;; from https://github.com/xenodium/agent-shell/issues/259
  (add-hook 'agent-shell-mode-hook (lambda () (setq-local doom-real-buffer-p t)))

  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
  (map! :localleader :map agent-shell-mode-map
        :desc "New session" "n"   #'agent-shell-restart
        :desc "Set Mode"    "TAB" #'agent-shell-set-session-mode
        :desc "Set Model"   "m"   #'agent-shell-set-session-model))
