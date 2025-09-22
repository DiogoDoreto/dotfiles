;;; tts.el --- Text-to-Speech (TTS) in Emacs -*- lexical-binding: t; -*-

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

;; This package provides Text-to-Speech (TTS) functionality for Emacs, enabling
;; users to convert textual content into audio playback. It supports streaming
;; TTS by splitting text into sentences and processing them sequentially,
;; allowing for smooth, real-time audio generation and playback without blocking
;; Emacs.

;; To use this package, first ensure the required external dependencies are
;; available: a TTS backend server (by default, Kokoro running in a Podman
;; container) and an audio player (by default, ffplay). Start the backend server
;; using `tts-kokoro-start-server', which launches a local Kokoro TTS service on
;; the configured port (`tts-kokoro-port' - default 8880).  Select text in a
;; buffer and call `tts-play' to initiate TTS playback; it handles the entire
;; region or buffer if no region is active.  Abort ongoing playback with
;; `tts-abort'.  Customize voices and speed using `tts-kokoro-set-voice' and
;; `tts-kokoro-set-speed'; prefix arguments make settings buffer-local.
;; Customize the port via `tts-kokoro-port'.  Logs and process outputs appear in
;; the *tts-log* buffer for debugging.

;; To extend this package, add new backends by defining generation functions
;; (e.g., like `tts--kokoro-generate-audio') and updating
;; `tts-backend-generate-function-alist'. Similarly, for frontends, define
;; playback functions (e.g., like `tts--ffplay-play-audio') and update
;; `tts-frontend-play-function-alist'. Ensure new backends implement the
;; expected callback interface for asynchronous processing.

;; You can find more information about Kokoro in this URL:
;; https://huggingface.co/hexgrad/Kokoro-82M
;; And its packaging into a container is provided by:
;; https://github.com/remsky/Kokoro-FastAPI

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

(defcustom tts-backend 'kokoro
  "The TTS backend currently in use for generating audio from text."
  :type 'symbol
  :group 'tts)

(defcustom tts-backend-generate-function-alist
  '((kokoro . tts--kokoro-generate-audio))
  "Alist mapping backends to their audio generation functions."
  :type 'alist
  :group 'tts)

(defcustom tts-frontend 'ffplay
  "The TTS frontend currently in use for playing audio files."
  :type 'symbol
  :group 'tts)

(defcustom tts-frontend-play-function-alist
  '((ffplay . tts--ffplay-play-audio))
  "Alist mapping frontends to their audio playback functions."
  :type 'alist
  :group 'tts)

(defcustom tts-kokoro-voice-list
  '("af_heart" "af_alloy" "af_aoede" "af_bella" "af_jessica" "af_kore"
    "af_nicole" "af_nova" "af_river" "af_sarah" "af_sky" "am_adam" "am_echo"
    "am_eric" "am_fenrir" "am_liam" "am_michael" "am_onyx" "am_puck" "am_santa"
    "bf_alice" "bf_emma"  "bf_isabell" "bf_lily" "bm_daniel" "bm_fable"
    "bm_george" "bm_lewis" "jf_alpha"   "jf_gongitsune" "jf_nezumi"
    "jf_tebukuro" "jm_kumo"  "zf_xiaobei" "zf_xiaoni" "zf_xiaoxiao" "zf_xiaoyi"
    "zm_yunjian" "zm_yunxi" "zm_yunxia" "zm_yunyang" "ef_dora" "em_alex"
    "em_santa" "ff_siwis" "hf_alpha" "hf_beta" "hm_omega" "hm_psi" "if_sara"
    "im_nicola" "pf_dora" "pm_alex" "pm_santa")
  "List of available voices for Kokoro TTS."
  :type '(repeat string)
  :group 'tts)

(defcustom tts-kokoro-voice (car tts-kokoro-voice-list)
  "The voice to use for Kokoro TTS."
  :type 'string
  :group 'tts)

(defcustom tts-kokoro-speed 1.1
  "The speed multiplier for Kokoro TTS."
  :type 'number
  :group 'tts)

(defcustom tts-kokoro-port 8880
  "The port number on which the Kokoro TTS server will run."
  :type 'integer
  :group 'tts)

(defvar tts--current-session nil
  "The active TTS session, instance of `tts--session', or nil if none.")

;;; Log helpers

(defun tts--log (prefix string)
  "Log a message with PREFIX to the TTS log buffer."
  (with-current-buffer (get-buffer-create "*tts-log*")
    (goto-char (point-max))
    (insert (format "[%s] %s\n" prefix string))))

(defun tts--logger-insertion-filter  (proc string)
  "Process filter that inserts logged strings into the process buffer, prefixed
with the process name."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (format "[%s] %s\n" (process-name proc) string))
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;;; Backend: kokoro

(defun tts-kokoro-start-server ()
  "Start Kokoro TTS server using Podman on the configured port."
  (interactive)
  (make-process
   :name "kokoro-server"
   :command `("podman" "run" "--replace" "--name" "kokoro-tts" "-d"
              ,(format "-p=%s:8880" tts-kokoro-port)
              "ghcr.io/remsky/kokoro-fastapi-cpu:latest")
   :noquery t
   :buffer (get-buffer-create "*tts-log*")
   :filter #'tts--logger-insertion-filter))

(defun tts--kokoro-format-voice (voice)
  "Format VOICE into a display string."
  (let* ((lang-char (aref voice 0))
         (gender-char (aref voice 1))
         (name (capitalize (substring voice 3)))
         (language (pcase lang-char
                     (?a "American English")
                     (?b "British English")
                     (?j "Japanese")
                     (?z "Mandarin Chinese")
                     (?e "Spanish")
                     (?f "French")
                     (?h "Hindi")
                     (?i "Italian")
                     (?p "Brazilian Portuguese")))
         (gender (if (eq gender-char ?f) "Female" "Male")))
    (format "[%s] [%s] %s" language gender name)))

(defun tts-kokoro-set-voice (arg)
  "Set the TTS voice for Kokoro. With prefix ARG, make it buffer-local."
  (interactive "P")
  (and-let* ((prompt (format "Select %s voice (currently %s):"
                             (if arg "buffer-local" "global")
                             (tts--kokoro-format-voice tts-kokoro-voice)))
             (voice-alist (cl-loop for voice in tts-kokoro-voice-list
                                   for formatted = (tts--kokoro-format-voice voice)
                                   collect (cons formatted voice)))
             (completion-table (lambda (string predicate action)
                                 (cond ((eq action 'metadata)
                                        `(metadata (display-sort-function . ,#'sort)))
                                       (t
                                        (complete-with-action action voice-alist string predicate)))))
             (fvoice (completing-read prompt completion-table nil t))
             (voice (alist-get fvoice voice-alist nil nil #'string=)))
    (if arg
        (progn
          (make-local-variable 'tts-kokoro-voice)
          (set 'tts-kokoro-voice voice)
          (message "Buffer-local TTS voice set to %s" voice))
      (customize-set-variable 'tts-kokoro-voice voice)
      (message "Global TTS voice set to %s" voice))))

(defun tts-kokoro-set-speed (arg)
  "Set the TTS speed for Kokoro. With prefix ARG, make it buffer-local."
  (interactive "P")
  (let ((speed (read-number (format "Set %s speed: " (if arg "buffer-local" "global"))
                            tts-kokoro-speed)))
    (if arg
        (progn
          (make-local-variable 'tts-kokoro-speed)
          (set 'tts-kokoro-speed speed)
          (message "Buffer-local TTS speed set to %s" speed))
      (customize-set-variable 'tts-kokoro-speed speed)
      (message "Global TTS speed set to %s" speed))))

(defun tts--kokoro-generate-audio (text file callback)
  "Generate audio for TEXT using the Kokoro TTS backend, saving to FILE. Call
CALLBACK with error or nil when done."
  (let* ((speech-url (format "http://localhost:%s/v1/audio/speech" tts-kokoro-port))
         (payload (json-encode
                   `(("model" . "kokoro")
                     ("input" . ,text)
                     ("voice" . ,tts-kokoro-voice)
                     ("lang_code" . ,(string (aref tts-kokoro-voice 0)))
                     ("response_format" . "mp3")
                     ("speed" . ,tts-kokoro-speed))))
         (cmd (list "curl" "-s" "-X" "POST"
                    "-H" "Content-Type: application/json"
                    "-d" (encode-coding-string payload 'utf-8)
                    "-o" file speech-url)))
    (make-process
     :name "tts-kokoro-curl"
     :command cmd
     :noquery t
     :buffer (get-buffer-create "*tts-log*")
     :filter #'tts--logger-insertion-filter
     :sentinel (lambda (_proc event)
                 (funcall callback
                          (unless (string= event "finished\n")
                            (string-trim event)))))))

;;; Frontend: ffplay

(defun tts--ffplay-play-audio (file callback)
  "Play the audio FILE using ffplay. Call CALLBACK with error or nil when
playback finishes."
  (make-process
   :name "tts-ffplay"
   :command (list "ffplay" "-nodisp" "-autoexit" file)
   :noquery t
   :buffer (get-buffer-create "*tts-log*")
   :filter #'tts--logger-insertion-filter
   :sentinel (lambda (_proc event)
               (funcall callback
                        (unless (string= event "finished\n")
                          (string-trim event))))))

;;; Data Structures

(cl-defstruct (tts--chunk (:constructor tts--chunk-create (index text)))
  "Represents a single unit of TTS synthesis and playback."
  index
  text
  (status 'pending :documentation "pending | requesting | ready | playing | done | error")
  (file nil :documentation "path to the generated audio file")
  (request-proc nil :documentation "Process for audio generation.")
  (audio-proc nil :documentation "Process for audio playback.")
  (error nil :documentation "Error string if status is error."))

(cl-defstruct (tts--session
               (:constructor tts--session-create
                             (text
                              &aux (chunks (cl-loop for sentence in (tts--split-into-sentences text)
                                                    for index from 0
                                                    collect (tts--chunk-create index sentence) into lst
                                                    finally return (vconcat lst))))))
  "Represents a multi-chunk TTS streaming session."
  (id (format-time-string "%Y%m%d%H%M%S"))
  (chunks nil :type vector :documentation "Vector of `tts--chunk'.")
  (aborted nil :documentation "t when this session has been aborted, nil otherwise."))

(defun tts--session-chunk (session index)
  "Return chunk at INDEX of SESSION or nil."
  (aref (tts--session-chunks session) index))

;;; Pre-processing

(defun tts--split-into-sentences (text)
  "Split TEXT into a list of sentences using Emacs' sentence navigation."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (sentences)
      (while (not (eobp))
        (let ((beg (point)))
          (forward-sentence)
          (let* ((raw (buffer-substring-no-properties beg (point)))
                 (trimmed (string-trim raw)))
            (when (> (length trimmed) 0)
              (push trimmed sentences)))))
      (nreverse sentences))))

;;; Processing

(defun tts--session-maybe-request-next-chunk ()
  "Request the next pending chunk in the current session if no chunk is
currently being requested and the session is not aborted."
  (unless (tts--session-aborted tts--current-session)
    (cl-loop for chunk across (tts--session-chunks tts--current-session)
             for status = (tts--chunk-status chunk)
             until (eq status 'requesting)
             if (eq status 'pending)
             return (tts--chunk-generate-audio chunk))))

(defun tts--session-maybe-play-next-chunk ()
  "If no chunk is currently playing and the session is not aborted, start
playing the next ready chunk."
  (unless (tts--session-aborted tts--current-session)
    (cl-loop for chunk across (tts--session-chunks tts--current-session)
             for status = (tts--chunk-status chunk)
             until (eq status 'playing)
             if (eq status 'ready)
             return (tts--chunk-play chunk))))

(defun tts--session-process-next-chunk ()
  "Advance the TTS session by attempting to play the next ready chunk and
request the next pending chunk if possible."
  (tts--session-maybe-play-next-chunk)
  (tts--session-maybe-request-next-chunk))

(defun tts--chunk-generate-audio (chunk)
  "Generate the audio file for CHUNK using the current TTS backend."
  (let* ((session tts--current-session)
         (generate-audio-function (alist-get tts-backend tts-backend-generate-function-alist))
         (chunk-text (tts--chunk-text chunk))
         (chunk-index (tts--chunk-index chunk))
         (temp-file (make-temp-file (format "tts-%s-%d-"
                                            (tts--session-id session)
                                            chunk-index)
                                    nil ".mp3")))
    (tts--log "tts--chunk-generate-audio"
              (format "Requesting chunk %d..." chunk-index))
    (setf (tts--chunk-file chunk) temp-file
          (tts--chunk-status chunk) 'requesting
          (tts--chunk-request-proc chunk)
          (funcall generate-audio-function chunk-text temp-file
                   (lambda (err)
                     (when (eq session tts--current-session)
                       (setf (tts--chunk-request-proc chunk) nil)
                       (cond
                        (err
                         (setf (tts--chunk-error chunk) err
                               (tts--chunk-status chunk) 'error)
                         (message "TTS: Error while generating audio for chunk %s. Check buffer *tts-log* for details."
                                  chunk-index))
                        (t
                         (setf (tts--chunk-status chunk) 'ready)
                         (tts--log "tts--chunk-generate-audio"
                                   (format "%s generated for chunk %d" temp-file
                                           chunk-index))))
                       (tts--session-process-next-chunk)))))))

(defun tts--chunk-play (chunk)
  "Play the audio file associated with CHUNK using the current TTS frontend."
  (message "TTS: Playing chunk %d..." (tts--chunk-index chunk))
  (let* ((session tts--current-session)
         (file (tts--chunk-file chunk))
         (play-function (alist-get tts-frontend tts-frontend-play-function-alist))
         (callback
          (lambda (err)
            (when (eq session tts--current-session)
              (setf (tts--chunk-audio-proc chunk) nil)
              (cond
               (err
                (setf (tts--chunk-status chunk) 'error
                      (tts--chunk-error chunk) (format "playback failed: %s" err))
                (message "TTS: Error while playing chunk %d. Check buffer *tts-log* for details." (tts--chunk-index chunk)))
               (t
                (setf (tts--chunk-status chunk) 'done)))
              (tts--session-process-next-chunk)))))
    (setf (tts--chunk-status chunk) 'playing
          (tts--chunk-audio-proc chunk) (funcall play-function file callback))))

(defun tts--chunk-abort (chunk)
  "Abort any ongoing processes for CHUNK and reset its status appropriately."
  (pcase (tts--chunk-status chunk)
    ('requesting
     (delete-process (tts--chunk-request-proc chunk))
     (setf (tts--chunk-request-proc chunk) nil)
     (setf (tts--chunk-status chunk) 'pending))
    ('playing
     (delete-process (tts--chunk-audio-proc chunk))
     (setf (tts--chunk-audio-proc chunk) nil)
     (setf (tts--chunk-status chunk) 'ready))))

;;; Interactive functions

(defun tts-play ()
  "Start a new TTS session for the selected region or the entire buffer if no
region is active.  The text is split into sentences and played sequentially as
audio chunks become available."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (setq text (string-trim text))
    (when (string-empty-p text)
      (user-error "TTS: No text to speak."))
    ;; Abort existing session (if any)
    (when (and tts--current-session
               (not (tts--session-aborted tts--current-session)))
      (tts-abort))
    ;; Create new session
    (setq tts--current-session (tts--session-create text))
    (message "TTS: Session %s started with %d chunk(s)."
             (tts--session-id tts--current-session)
             (length (tts--session-chunks tts--current-session)))
    (tts--session-maybe-request-next-chunk)))

(defun tts-abort ()
  "Abort the current TTS session if one is active."
  (interactive)
  (unless (tts--session-aborted tts--current-session)
    (setf (tts--session-aborted tts--current-session) t)
    (mapc #'tts--chunk-abort (tts--session-chunks tts--current-session))
    (when (called-interactively-p 'interactive)
      (message "TTS: Session %s aborted." (tts--session-id tts--current-session)))))

(provide 'tts)
;;; tts.el ends here
