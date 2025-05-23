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

(use-package! gptel
  :defer t
  :config
  (load! "my-gptel-tools.el")

  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))
  (and-let* ((auth-item (auth-source-search :host "github.com"))
             (token (funcall (plist-get (car auth-item) :secret))))
    (gptel-make-openai "Free Copilot"
      :host "models.inference.ai.azure.com"
      :endpoint "/chat/completions?api-version=2024-12-01-preview"
      :stream t
      :key token
      :models '(gpt-4o o1 DeepSeek-R1)))

  (setq gptel-model 'gpt-4o
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-confirm-tool-calls t))

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
      (:prefix-map ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort"             "x" #'gptel-abort

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode

       :desc "model=gpt-4o"  "1" (cmd! (setq gptel-model 'gpt-4o))
       :desc "model=o1"      "2" (cmd! (setq gptel-model 'o1))
       :desc "model=gpt-4.1" "3" (cmd! (setq gptel-model 'gpt-4.1))
       :desc "model=claude-3.7-sonnet"         "4" (cmd! (setq gptel-model 'claude-3.7-sonnet))
       :desc "model=claude-3.7-sonnet-thought" "5" (cmd! (setq gptel-model 'claude-3.7-sonnet-thought))

       :desc "Aider" "d" #'aidermacs-transient-menu

       :desc "Whisper Run"       "w" #'whisper-run
       :desc "Whisper File"      "W" #'whisper-file))

(setq whisper-return-cursor-to-start nil)

(defun dd/fill-after-whisper ()
  "Auto wrap the long generated line from Whisper"
  (evil-fill (line-beginning-position) (line-end-position)))

(add-hook 'whisper-after-insert-hook #'dd/fill-after-whisper)
