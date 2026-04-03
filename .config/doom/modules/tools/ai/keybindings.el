;;; keybindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort"             "x" #'gptel-abort
       :desc "Rewrite region" :v "r" #'gptel-rewrite
       :desc "Setup File System MCP" "f" #'+mcp/prepare-filesystem

       :desc "Agent shell" "s" #'agent-shell

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode

       :desc "model=gpt-5-mini"        "1" (cmd! (setq gptel-model 'gpt-5-mini))
       :desc "model=gpt-5.2"           "2" (cmd! (setq gptel-model 'gpt-5.2))
       :desc "model=gpt-5.2-codex"     "3" (cmd! (setq gptel-model 'gpt-5.2-codex))
       :desc "model=claude-sonnet-4.6" "4" (cmd! (setq gptel-model 'claude-sonnet-4.6))
       :desc "model=claude-opus-4.6"   "5" (cmd! (setq gptel-model 'claude-opus-4.6))))

(map! :leader
      (:prefix ("l ." . "OneShot cmds")
       :desc "Create commit"         "c" #'dd/gptel-create-commit
       :desc "Review staged changes" "r" #'dd/gptel-code-review-staged-changes))
