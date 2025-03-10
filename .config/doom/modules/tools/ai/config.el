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
  (setq gptel-model 'qwen2.5-coder:32b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen2.5-coder:32b deepseek-r1:32b))
        gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))
  (and-let* ((auth-item (auth-source-search :host "github.com"))
             (token (funcall (plist-get (car auth-item) :secret))))
    (setq gptel-model 'gpt-4o
          gptel-backend (gptel-make-openai "Copilot"
                          :host "models.inference.ai.azure.com"
                          :endpoint "/chat/completions?api-version=2024-12-01-preview"
                          :stream t
                          :key token
                          :models '(gpt-4o o1 DeepSeek-R1)))))

(map! :leader
      (:prefix-map ("l" . "LLM")
       :desc "Open chat buffer" "o" #'gptel
       :desc "Send" "RET" #'gptel-send
       :desc "Menu" "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort" "x" #'gptel-abort

       :desc "Whisper Run" "w" #'whisper-run
       :desc "Whisper File" "W" #'whisper-file))

(setq whisper-return-cursor-to-start nil)

(defun dd/fill-after-whisper ()
  "Auto wrap the long generated line from Whisper"
  (evil-fill (line-beginning-position) (line-end-position)))

(add-hook 'whisper-after-insert-hook #'dd/fill-after-whisper)
