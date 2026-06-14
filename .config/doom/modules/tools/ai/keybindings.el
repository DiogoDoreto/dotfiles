;;; keybindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Rewrite region" :v "r" #'gptel-rewrite

       :desc "Agent shell" "s" #'agent-shell

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode))

(map! :leader
      (:prefix ("l ." . "OneShot cmds")
       :desc "Create commit"         "c" #'dd/gptel-create-commit
       :desc "Review staged changes" "r" #'dd/gptel-code-review-staged-changes))

(map! :after gptel
      :localleader
      :map gptel-mode-map
      :desc "Set model" "m" #'+gptel-set-model
      :desc "Abort"     "x" #'gptel-abort)
