;;; -*- lexical-binding: t; -*-
(after! gptel
  (setq gptel-model 'qwen2.5-coder:32b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "chungus.home:11434"
                        :stream t
                        :models '(qwen2.5-coder:32b deepseek-r1:32b qwq))))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
