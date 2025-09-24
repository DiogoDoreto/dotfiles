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
;; region or buffer if no region is active.  Stop ongoing playback with
;; `tts-stop'.  Customize voices and speed using `tts-kokoro-set-voice' and
;; `tts-kokoro-set-speed'; prefix arguments make settings buffer-local.
;; Customize the port via `tts-kokoro-port'.  Logs and process outputs appear in
;; the *tts-log* buffer for debugging.  When you start playing, `tts-mode' will
;; be enabled in that buffer to display a header-line showing playback progress
;; and controls.

;; To extend this package, add new backends by defining generation functions
;; (e.g., like `tts--kokoro-generate-audio') and updating
;; `tts-backend-generate-function-alist'. Similarly, for frontends, define
;; playback functions (e.g., like `tts--ffplay-play-audio') and update
;; `tts-frontend-play-function-alist'. Ensure new backends and frontends
;; implement the expected callback interface for asynchronous processing.  For
;; backend-specific UI controls in the header line, define a UI function (e.g.,
;; like `tts--kokoro-ui-controls') and update
;; `tts-backend-ui-controls-function-alist'. For preprocessing, define
;; transformation functions (e.g., like `tts--preprocess-org-links') and add
;; them to `tts-preprocessing-functions'.  These apply per sentence before TTS
;; generation.

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

(defcustom tts-backend-ui-controls-function-alist
  '((kokoro . tts--kokoro-ui-controls))
  "Alist mapping backends to their UI controls rendering functions for the
header line. The function must return a string."
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

(defcustom tts-preprocessing-functions '(tts--preprocess-org-links)
  "List of functions to preprocess TTS text per sentence.

Each function takes a string (sentence text) and returns the transformed string."
  :type '(repeat function)
  :group 'tts)

(defface tts-highlight-face
  '((t (:inherit highlight)))
  "Face for highlighting sentences being spoken by TTS."
  :group 'tts)

(defcustom tts-enable-highlighting t
  "Enable highlighting of sentences being spoken by TTS."
  :type 'boolean
  :group 'tts)

(defcustom tts-enable-autoscroll t
  "Enable automatic scroll of the buffer window to keep spoken text visible."
  :type 'boolean
  :group 'tts)

(defvar tts--current-session nil
  "The active TTS session, instance of `tts--session', or nil if none.")

(defvar-local tts--region-active-p nil
  "Buffer-local flag tracking if a region is active for header-line updates.")

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
      (message "Global TTS voice set to %s" voice))
    (tts--update-header-line)))

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
      (message "Global TTS speed set to %s" speed))
    (tts--update-header-line)))

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

(defun tts--kokoro-ui-controls ()
  "Return a string of UI controls for the Kokoro TTS backend."
  (concat
   (buttonize (format "[Voice: %s]" tts-kokoro-voice)
              (lambda (_) (let ((current-prefix-arg '(4))) (call-interactively 'tts-kokoro-set-voice)))
              nil "Change TTS voice")
   " "
   (buttonize (format "[Speed: %s]" tts-kokoro-speed)
              (lambda (_) (let ((current-prefix-arg '(4))) (call-interactively 'tts-kokoro-set-speed)))
              nil "Change TTS speed")))

;;; Frontend: ffplay

(defun tts--ffplay-play-audio (file callback)
  "Play the audio FILE using ffplay. Call CALLBACK with error or nil when
playback finishes."
  (make-process
   :name "tts-ffplay"
   :command (list "ffplay" "-nodisp" "-autoexit" "-v" "level+error" file)
   :noquery t
   :buffer (get-buffer-create "*tts-log*")
   :filter #'tts--logger-insertion-filter
   :sentinel (lambda (_proc event)
               (funcall callback
                        (unless (string= event "finished\n")
                          (string-trim event))))))

;;; Data Structures

(cl-defstruct (tts--chunk (:constructor tts--chunk-create (index text &optional beg end)))
  "Represents a single unit of TTS synthesis and playback."
  index
  text
  (beg nil :documentation "Point in session's buffer where this chunk begins. Used for highlighting.")
  (end nil :documentation "Point in session's buffer where this chunk ends. Used for highlighting.")
  (status 'pending :documentation "pending | requesting | ready | playing | error")
  (file nil :documentation "path to the generated audio file")
  (request-proc nil :documentation "Process for audio generation.")
  (audio-proc nil :documentation "Process for audio playback.")
  (overlay nil :documentation "Overlay for highlighting the sentence.")
  (error nil :documentation "Error string if status is error."))

(cl-defstruct (tts--session
               (:constructor tts--session-create
                             (sentences
                              buffer
                              &aux (chunks (cl-loop for sentence in sentences
                                                    for index from 0
                                                    collect (apply #'tts--chunk-create index sentence)
                                                    into lst
                                                    finally return (vconcat lst))))))
  "Represents a multi-chunk TTS streaming session."
  (id (format-time-string "%Y%m%d%H%M%S"))
  (current-chunk-index 0 :documentation "Index of currently playing chunk.")
  (status 'playing :documentation "playing | paused | stopped. Indicates session status.")
  (buffer nil :documentation "Buffer where TTS is playing.")
  (chunks nil :type vector :documentation "Vector of `tts--chunk'."))

(defun tts--current-session-active-p ()
  "Returns t when a current session exists and it has not been stopped."
  (and tts--current-session
       (not (eq (tts--session-status tts--current-session) 'stopped))))

(defun tts--session-chunk (index)
  "Return chunk at INDEX of current session or nil."
  (aref (tts--session-chunks tts--current-session) index))

(defun tts--current-chunk ()
  (when tts--current-session
    (when-let ((current-index (tts--session-current-chunk-index tts--current-session)))
      (tts--session-chunk current-index))))

;;; Pre-processing

(defun tts--preprocess-org-links (text)
  "Replace org-mode links [[url][description]] with description in TEXT."
  (replace-regexp-in-string (rx "[[" (+? nonl) "][" (group (+? nonl)) "]")
                            "\\1" text))

(defun tts--split-into-sentences (beg end)
  "Split the text between BEG and END into a list of (text beg end) triples for
each sentence."
  (let (sentences)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((sentence-beg (point)))
          (forward-sentence)
          (let ((sentence-end (min (point) end)))
            (let ((actual-beg sentence-beg)
                  (actual-end sentence-end))
              (save-excursion
                (goto-char sentence-beg)
                (skip-chars-forward " \t\n\r" sentence-end)
                (setq actual-beg (point))
                (goto-char sentence-end)
                (skip-chars-backward " \t\n\r" sentence-beg)
                (setq actual-end (point)))
              (let ((text (buffer-substring-no-properties actual-beg actual-end)))
                (dolist (func tts-preprocessing-functions)
                  (setq text (funcall func text)))
                (when (> (length text) 0)
                  (push (list text actual-beg actual-end) sentences))))))))
    (nreverse sentences)))

;;; Processing

(defun tts--session-maybe-request-next-chunk ()
  "Request the next pending chunk in the current session if no chunk is
currently being requested and the session is not stopped."
  (when (tts--current-session-active-p)
    (cl-loop for chunk across (tts--session-chunks tts--current-session)
             for status = (tts--chunk-status chunk)
             until (eq status 'requesting)
             if (eq status 'pending)
             return (tts--chunk-generate-audio chunk))))

(defun tts--session-process-next-chunk ()
  "Advance the TTS session by requesting the next pending chunk if possible."
  (tts--session-maybe-request-next-chunk)
  (tts--update-header-line))

(defun tts--chunk-cleanup-overlay (chunk)
  "Clean up the overlay for CHUNK if it exists."
  (when-let ((ov (tts--chunk-overlay chunk)))
    (delete-overlay ov)
    (setf (tts--chunk-overlay chunk) nil)))

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
                                  chunk-index)
                         (when (= chunk-index (tts--session-current-chunk-index session))
                           (tts-next)))
                        (t
                         (setf (tts--chunk-status chunk) 'ready)
                         (tts--log "tts--chunk-generate-audio"
                                   (format "%s generated for chunk %d" temp-file
                                           chunk-index))
                         (when (= chunk-index (tts--session-current-chunk-index session))
                           (tts-play))))
                       (tts--session-process-next-chunk)))))))

(defun tts--chunk-play (chunk)
  "Play the audio file associated with CHUNK using the current TTS frontend."
  (let* ((session tts--current-session)
         (play-function (alist-get tts-frontend tts-frontend-play-function-alist))
         (callback
          (lambda (err)
            (when (eq session tts--current-session)
              (setf (tts--chunk-audio-proc chunk) nil)
              (tts--chunk-cleanup-overlay chunk)
              (cond
               (err
                (setf (tts--chunk-status chunk) 'error
                      (tts--chunk-error chunk) (format "playback failed: %s" err))
                (message "TTS: Error while playing chunk %d. Check buffer *tts-log* for details." (tts--chunk-index chunk)))
               (t
                (setf (tts--chunk-status chunk) 'ready)
                (tts-next)))))))
    (pcase-let (((cl-struct tts--session buffer) session)
                ((cl-struct tts--chunk file beg end) chunk))
      (setf (tts--session-status session) 'playing)
      (setf (tts--chunk-status chunk) 'playing
            (tts--chunk-overlay chunk) (when (and tts-enable-highlighting (buffer-live-p buffer) beg end)
                                         (let ((overlay (make-overlay beg end buffer)))
                                           (overlay-put overlay 'face 'tts-highlight-face)
                                           overlay))
            (tts--chunk-audio-proc chunk) (funcall play-function file callback))
      ;; Ensure the playing chunk is visible in its window if possible
      (when (and tts-enable-autoscroll (buffer-live-p buffer))
        (when-let ((win (get-buffer-window buffer t)))
          (with-selected-window win
            (unless (pos-visible-in-window-p end win)
              (goto-char beg)
              (recenter 3))))))))

(defun tts--chunk-stop (chunk)
  "Stop any ongoing processes for CHUNK and reset its status appropriately."
  (tts--chunk-cleanup-overlay chunk)
  (pcase (tts--chunk-status chunk)
    ('requesting
     (delete-process (tts--chunk-request-proc chunk))
     (setf (tts--chunk-request-proc chunk) nil)
     (setf (tts--chunk-status chunk) 'pending))
    ('playing
     (delete-process (tts--chunk-audio-proc chunk))
     (setf (tts--chunk-audio-proc chunk) nil)
     (setf (tts--chunk-status chunk) 'ready))))

(defun tts--play-chunk-at (index)
  "Plays the chunk at INDEX as long as it's ready. Will stop the current chunk
if it's playing."
  (let ((chunk (tts--session-chunk index)))
    (when-let ((current-chunk (tts--current-chunk)))
      (when (eq (tts--chunk-status current-chunk) 'playing)
        (tts--chunk-stop current-chunk)))
    (setf (tts--session-current-chunk-index tts--current-session) index)
    (when (eq (tts--chunk-status chunk) 'ready)
      (tts--chunk-play chunk))
    (tts--update-header-line)))

;;; tts-mode

(define-minor-mode tts-mode
  "Toggle TTS header-line display."
  :init-value nil
  :lighter " TTS"
  :global nil
  (if tts-mode
      (progn
        (unless (local-variable-p 'tts--original-header-line-format)
          (set (make-local-variable 'tts--original-header-line-format) header-line-format))
        (set (make-local-variable 'tts--region-active-p) (use-region-p))
        (add-hook 'post-command-hook #'tts--check-region-change nil t)
        (tts--update-header-line))
    (when (local-variable-p 'tts--original-header-line-format)
      (setq header-line-format tts--original-header-line-format)
      (kill-local-variable 'tts--original-header-line-format))
    (when (local-variable-p 'tts--region-active-p)
      (kill-local-variable 'tts--region-active-p))
    (remove-hook 'post-command-hook #'tts--check-region-change t)))

(defun tts--check-region-change ()
  "Check if the region selection status has changed and update header-line if
necessary."
  (let ((active (use-region-p)))
    (when (and tts-mode (not (eq active tts--region-active-p)))
      (setq tts--region-active-p active)
      (tts--update-header-line))))

(defun tts--header-line-format ()
  "Return the list for the TTS header-line with left and right aligned parts."
  (let* ((playing nil)
         (requesting nil)
         (total 0)
         (status 'stopped))
    (when tts--current-session
      (let ((chunks (tts--session-chunks tts--current-session)))
        (setq total (length chunks)
              playing (1+ (tts--session-current-chunk-index tts--current-session))
              status (tts--session-status tts--current-session)
              requesting (cl-loop for chunk across chunks
                                  for position from 1
                                  if (eq 'requesting (tts--chunk-status chunk))
                                  return position))))
    (let ((left-part (concat
                      (propertize " /TTS/ " 'face 'bold)
                      (buttonize (format "[Read %s]" (if (use-region-p) "Region" "Buffer"))
                                 (lambda (_) (tts-read)))
                      (when (and (not (eq status 'stopped))
                                 (> total 0))
                        (concat " | "
                                (if (> playing 1)
                                    (buttonize "[Prev]" (lambda (_) (tts-prev)))
                                  (propertize "[Prev]" 'face 'shadow))
                                " "
                                (if (eq status 'playing)
                                    (buttonize "[Pause]" (lambda (_) (tts-pause)))
                                  (buttonize "[Play]" (lambda (_) (tts-play))))
                                " "
                                (if (< playing total)
                                    (buttonize "[Next]" (lambda (_) (tts-next)))
                                  (propertize "[Next]" 'face 'shadow))
                                "  "
                                (buttonize "[Stop]" (lambda (_) (tts-stop)) nil "Stop TTS playback and prefetching")
                                (format " | %s" playing)
                                (when requesting (format " / %s" requesting))
                                (format " / %d" total)))))
          (right-part (when-let ((backend-controls (alist-get tts-backend tts-backend-ui-controls-function-alist)))
                        (concat (funcall backend-controls)
                                " "))))
      (list left-part
            (propertize " " 'display `(space :align-to (- right ,(length right-part))))
            right-part))))

(defun tts--update-header-line ()
  "Update the header-line in all buffers where tts-mode is active."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when tts-mode
        (setq header-line-format (tts--header-line-format))))))

;;; Interactive functions

(defun tts-read ()
  "Start a new TTS session for the selected region or the entire buffer if no
region is active.  The text is split into sentences and played sequentially as
audio chunks become available."
  (interactive)
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (sentences (tts--split-into-sentences beg end)))
    (when (use-region-p)
      (deactivate-mark))
    (unless sentences
      (user-error "TTS: No text to speak."))
    (tts-stop)
    (setq tts--current-session (tts--session-create sentences (current-buffer)))
    (message "TTS: Session %s started with %d chunk(s)."
             (tts--session-id tts--current-session)
             (length (tts--session-chunks tts--current-session)))
    (tts--session-maybe-request-next-chunk)
    (if tts-mode
        (tts--update-header-line)
      (tts-mode 1))))

(defun tts-stop ()
  "Stop the current TTS session if one is active."
  (interactive)
  (when (tts--current-session-active-p)
    (setf (tts--session-status tts--current-session) 'stopped)
    (mapc #'tts--chunk-stop (tts--session-chunks tts--current-session))
    (when (called-interactively-p 'interactive)
      (message "TTS: Session stopped."))
    (tts--update-header-line)))

(defun tts-play ()
  "Play or resume the current chunk in the TTS session."
  (interactive)
  (when (tts--current-session-active-p)
    (tts--play-chunk-at (tts--session-current-chunk-index tts--current-session))))

(defun tts-pause ()
  "Pause the current TTS playback."
  (interactive)
  (when (tts--current-session-active-p)
    (setf (tts--session-status tts--current-session) 'paused)
    (when-let ((chunk (tts--current-chunk)))
      (when (eq (tts--chunk-status chunk) 'playing)
        (tts--chunk-stop chunk)))
    (tts--update-header-line)))

(defun tts-next ()
  "Play the next chunk in the TTS session."
  (interactive)
  (when (tts--current-session-active-p)
    (let* ((max-index (1- (length (tts--session-chunks tts--current-session))))
           (current-index (tts--session-current-chunk-index tts--current-session))
           (next-index (min (1+ current-index) max-index)))
      (if (/= current-index next-index)
          (tts--play-chunk-at next-index)
        (setf (tts--session-status tts--current-session) 'paused)
        (tts--update-header-line)))))

(defun tts-prev ()
  "Play the previous chunk in the TTS session."
  (interactive)
  (when (tts--current-session-active-p)
    (let* ((current-index (tts--session-current-chunk-index tts--current-session))
           (prev-index (max 0 (1- current-index))))
      (tts--play-chunk-at prev-index))))

(provide 'tts)
;;; tts.el ends here
