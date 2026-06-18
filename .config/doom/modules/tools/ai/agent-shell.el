;;; agent-shell.el -*- lexical-binding: t; -*-

(use-package agent-shell
  :defer t
  :after acp
  :config
  (setopt agent-shell-session-strategy 'prompt
          agent-shell-show-usage-at-turn-end t
          agent-shell-context-sources '(files region error)
          agent-shell-busy-indicator-frames 'dots-block
          agent-shell-session-restore-verbosity 'full)

  (when (string= (system-name) "lapdog")
    (setopt agent-shell-agent-configs (list (agent-shell-openai-make-codex-config)
                                            (agent-shell-opencode-make-agent-config))
            agent-shell-preferred-agent-config (car agent-shell-agent-configs)
            ;; env var fixes init issue https://github.com/agentclientprotocol/claude-agent-acp/issues/575
            agent-shell-anthropic-claude-acp-command '("ssh" "lapdog-agent" "CLAUDE_CODE_EXECUTABLE=/etc/profiles/per-user/dog/bin/claude" "claude-agent-acp")))

  (when (string= (system-name) "DT-5RHWB24")
    (setopt agent-shell-agent-configs (list (agent-shell-anthropic-make-claude-code-config)
                                            (agent-shell-opencode-make-agent-config))
            agent-shell-opencode-acp-command '("opencode" "acp" "--attach" "http://localhost:4242")))

  (defun +dd/agent-shell--on-idle (event)
    "Send an OS notification when agent is idle"
    (let* ((buf (map-nested-elt event '(:data :buffer)))
           (win (get-buffer-window buf))
           (frame (and win (window-frame win))))
      (message "agent-shell IDLE")
      (unless (and frame (eq (frame-focus-state frame) t))
        (let ((msg (format "%s is waiting" (buffer-name buf))))
          (start-process "notify" nil "notify-send"
                         "--icon=emacs" "Agent Shell" msg)))))

  (add-hook 'agent-shell-mode-hook
            (lambda ()
              ;; from https://github.com/xenodium/agent-shell/issues/259
              (setq-local doom-real-buffer-p t)
              (agent-shell-subscribe-to
               :shell-buffer (current-buffer)
               :event 'idle
               :on-event #'+dd/agent-shell--on-idle)))

  (map! :map agent-shell-mode-map
        :ni "<up>" #'previous-line
        :ni "<down>" #'next-line
        :i "C-u" #'evil-delete-back-to-indentation
        :n "g h" #'agent-shell-ui-backward-block
        :n "g l" #'agent-shell-ui-forward-block
        :n "g s" nil
        :n "<return>" (cmds! (agent-shell-ui--enclosing-fragment-position)
                             #'agent-shell-ui-toggle-fragment
                             #'shell-maker-submit))

  (map! :localleader :map agent-shell-mode-map
        :desc "New session"       "n"   #'agent-shell-restart
        :desc "Fork session"      "f"   #'agent-shell-fork
        :desc "Rename buffer"     "r"   #'agent-shell-rename-buffer
        :desc "Put shell cmd"     "x"   #'agent-shell-insert-shell-command-output
        :desc "Paste image"       "i"   #'agent-shell-send-clipboard-image
        :desc "Set Mode"          "TAB" #'agent-shell-set-session-mode
        :desc "Set Model"         "m"   #'agent-shell-set-session-model
        :desc "Set Thought Level" "l"   #'agent-shell-set-session-thought-level))
