;;; -*- lexical-binding: t; -*-
(after! gptel
  (gptel-make-ollama "Ollama"
    :host "chungus.home:11434"
    :stream t
    :models '(qwen2.5-coder:32b deepseek-r1:32b qwq)))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun whisper-command (input-file)
  "Produces whisper command to be run on the INPUT-FILE."
  `(,(whisper--find-whispercpp-main)
    "--model" ,whisper-model
    ,input-file))
