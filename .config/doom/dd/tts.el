;;; tts.el --- TTS functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Diogo Doreto

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  TTS functions

;;; Code:

(require 'url)
(require 'json)

(defun tts-start-server ()
  "Start the Kokoro TTS server with podman."
  (interactive)
  (start-process-shell-command
   "kokoro-tts" nil
   "podman run --name kokoro-tts -d -p 8880:8880 ghcr.io/remsky/kokoro-fastapi-cpu:latest"))

(defvar tts--request-process nil
  "Process object for the ongoing TTS HTTP request (curl).")

(defvar tts--audio-process nil
  "Process object for the ongoing audio playback (ffplay).")

(defun tts (input-text)
  "Generate speech from INPUT-TEXT, save as mp3 in a temp directory, and play it
with ffplay. Uses curl so request can be aborted."
  (let* ((speech-url "http://localhost:8880/v1/audio/speech")
         (payload (json-encode
                   `(("model" . "kokoro")
                     ("input" . ,input-text)
                     ("voice" . "af_bella")
                     ("response_format" . "mp3")
                     ("speed" . 1.1))))
         (temp-file (make-temp-file "tts-" nil ".mp3"))
         (curl-cmd (list "curl" "-s" "-X" "POST"
                         "-H" "Content-Type: application/json"
                         "-d" (encode-coding-string payload 'utf-8)
                         "-o" temp-file speech-url)))
    (message "Generating TTS...")
    ;; Abort previous request/audio
    (when (process-live-p tts--request-process)
      (delete-process tts--request-process)
      (setq tts--request-process nil))
    (when (process-live-p tts--audio-process)
      (delete-process tts--audio-process)
      (setq tts--audio-process nil))
    (setq tts--request-process
          (make-process :name "tts-curl"
                        :buffer (generate-new-buffer " *tts-curl*")
                        :command curl-cmd
                        :noquery t
                        :sentinel (lambda (proc event)
                                    (when (string= event "finished\n")
                                      (kill-buffer (process-buffer proc))
                                      (setq tts--request-process nil)
                                      (message "Playing TTS...")
                                      (setq tts--audio-process
                                            (start-process "ffplay" nil "ffplay" "-nodisp" "-autoexit" temp-file))))))))

(defun tts-play ()
  "Speak the selected region if active, otherwise the entire buffer using TTS."
  ;; TODO split text into sentences and queue them, so that we can start playing
  ;; sooner and buffer the future ones during playback.
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (tts text)))

(defun tts-abort ()
  "Abort TTS HTTP request or stop ongoing audio playback."
  (interactive)
  (let ((req-aborted nil)
        (audio-aborted nil))
    (when (and tts--request-process (process-live-p tts--request-process))
      (delete-process tts--request-process)
      (setq tts--request-process nil)
      (setq req-aborted t))
    (when (and tts--audio-process (process-live-p tts--audio-process))
      (delete-process tts--audio-process)
      (setq tts--audio-process nil)
      (setq audio-aborted t))
    (cond
     ((and req-aborted audio-aborted)
      (message "Aborted TTS request and stopped audio playback."))
     (req-aborted
      (message "Aborted TTS request."))
     (audio-aborted
      (message "Stopped audio playback."))
     (t
      (message "No active TTS request or audio playback to abort.")))))

(provide 'tts)
;;; tts.el ends here
