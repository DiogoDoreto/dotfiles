;;; agent-shell.el -*- lexical-binding: t; -*-

(use-package agent-shell
  :defer t
  :after acp
  :config
  (setopt agent-shell-session-strategy 'prompt
          agent-shell-show-usage-at-turn-end t
          agent-shell-context-sources '(files region error)
          agent-shell-agent-configs (list (agent-shell-anthropic-make-claude-code-config)
                                          (agent-shell-opencode-make-agent-config)))

  (when (string= (system-name) "lapdog")
    (setopt agent-shell-preferred-agent-config (car agent-shell-agent-configs)
            ;; env var fixes init issue https://github.com/agentclientprotocol/claude-agent-acp/issues/575
            agent-shell-anthropic-claude-acp-command '("ssh" "lapdog-agent" "CLAUDE_CODE_EXECUTABLE=/etc/profiles/per-user/dog/bin/claude" "claude-agent-acp")))

  (when (string= (system-name) "DT-5RHWB24")
    (setopt agent-shell-opencode-acp-command '("opencode" "acp" "--attach" "http://localhost:4242")))

  ;; from https://github.com/xenodium/agent-shell/issues/259
  (add-hook 'agent-shell-mode-hook (lambda () (setq-local doom-real-buffer-p t)))

  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
  (map! :localleader :map agent-shell-mode-map
        :desc "New session" "n"   #'agent-shell-restart
        :desc "Set Mode"    "TAB" #'agent-shell-set-session-mode
        :desc "Set Model"   "m"   #'agent-shell-set-session-model))
