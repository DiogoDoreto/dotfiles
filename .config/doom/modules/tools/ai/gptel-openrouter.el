;;; gptel-openrouter.el -*- lexical-binding: t; -*-

;;; OpenRouter metadata cache

(defvar gptel--known-backends)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(declare-function gptel--model-name "gptel-request")
(declare-function gptel-backend-models "gptel-request")

(defvar +gptel-openrouter-backend-name "OpenRouter")
(defvar +gptel-openrouter-models-url "https://openrouter.ai/api/v1/models")
(defvar +gptel-openrouter-cache-file
  (expand-file-name "gptel/openrouter-models.json" doom-cache-dir))
(defvar +gptel-openrouter-cache-ttl (days-to-time 7))
(defvar +gptel-openrouter--refresh-in-flight nil)

(defun +gptel-openrouter--cache-fresh-p ()
  "Return non-nil when the OpenRouter metadata cache is fresh."
  (when-let* ((attrs (file-attributes +gptel-openrouter-cache-file))
              (mtime (file-attribute-modification-time attrs)))
    (time-less-p (time-subtract (current-time) mtime)
                 +gptel-openrouter-cache-ttl)))

(defun +gptel-openrouter--parse-json (json)
  "Parse OpenRouter JSON into alists."
  (require 'json)
  (json-parse-string json
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun +gptel-openrouter--read-cache ()
  "Read cached OpenRouter model metadata."
  (when (file-readable-p +gptel-openrouter-cache-file)
    (let ((coding-system-for-read 'utf-8-unix))
      (with-temp-buffer
        (insert-file-contents +gptel-openrouter-cache-file)
        (+gptel-openrouter--parse-json (buffer-string))))))

(defun +gptel-openrouter--write-cache (json)
  "Write raw OpenRouter model metadata JSON to cache."
  (make-directory (file-name-directory +gptel-openrouter-cache-file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file +gptel-openrouter-cache-file
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert json))))

(defun +gptel-openrouter--number (value)
  "Return VALUE as a number when it looks numeric."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p
          "\\`[[:space:]]*-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?[[:space:]]*\\'"
          value))
    (string-to-number value))))

(defun +gptel-openrouter--price-per-million (price)
  "Convert OpenRouter per-token PRICE to gptel's per-million-token unit."
  (when-let* ((number (+gptel-openrouter--number price)))
    (when (>= number 0)
      (* number 1000000.0))))

(defun +gptel-openrouter--context-window (context-length)
  "Convert OpenRouter CONTEXT-LENGTH to gptel's thousands-of-tokens unit."
  (when-let* ((number (+gptel-openrouter--number context-length)))
    (/ number 1000.0)))

(defun +gptel-openrouter--models-by-id (payload)
  "Return a hash table of OpenRouter model metadata from PAYLOAD."
  (let ((models (make-hash-table :test #'equal)))
    (dolist (model (alist-get 'data payload))
      (when-let* ((id (alist-get 'id model)))
        (puthash id model models))
      (when-let* ((canonical-slug (alist-get 'canonical_slug model)))
        (puthash canonical-slug model models)))
    models))

(defun +gptel-openrouter--backend ()
  "Return the configured OpenRouter gptel backend, if present."
  (when (boundp 'gptel--known-backends)
    (alist-get +gptel-openrouter-backend-name
               gptel--known-backends nil nil #'equal)))

(defun +gptel-openrouter--model-name (model)
  "Return MODEL's provider-facing name."
  (if (fboundp 'gptel--model-name)
      (gptel--model-name model)
    (if (symbolp model) (symbol-name model) model)))

(defun +gptel-openrouter--apply-model (model metadata)
  "Apply OpenRouter METADATA to gptel MODEL."
  (let* ((pricing (alist-get 'pricing metadata))
         (description (alist-get 'description metadata))
         (context-window
          (+gptel-openrouter--context-window
           (alist-get 'context_length metadata)))
         (input-cost
          (+gptel-openrouter--price-per-million
           (alist-get 'prompt pricing)))
         (output-cost
          (+gptel-openrouter--price-per-million
           (alist-get 'completion pricing)))
         (cutoff-date (alist-get 'knowledge_cutoff metadata)))
    (put model :description description)
    (put model :context-window context-window)
    (put model :input-cost input-cost)
    (put model :output-cost output-cost)
    (put model :cutoff-date cutoff-date)))

(defun +gptel-openrouter--apply-model-metadata (payload)
  "Apply cached or fetched OpenRouter model metadata from PAYLOAD."
  (when-let* ((backend (+gptel-openrouter--backend)))
    (let ((models-by-id (+gptel-openrouter--models-by-id payload))
          (updated 0))
      (dolist (model (gptel-backend-models backend))
        (when-let* ((metadata (gethash (+gptel-openrouter--model-name model)
                                       models-by-id)))
          (+gptel-openrouter--apply-model model metadata)
          (setq updated (1+ updated))))
      updated)))

(defun +gptel-openrouter--response-body ()
  "Return the current `url-retrieve' buffer body."
  (goto-char (or (and (boundp 'url-http-end-of-headers)
                      url-http-end-of-headers)
                 (point-min)))
  (unless (or (bound-and-true-p url-http-end-of-headers)
              (re-search-forward "\r?\n\r?\n" nil t))
    (user-error "Could not find OpenRouter response body"))
  (buffer-substring-no-properties (point) (point-max)))

(defun +gptel-openrouter--fetch-model-metadata ()
  "Fetch OpenRouter model metadata, cache it, and apply it to gptel models."
  (unless +gptel-openrouter--refresh-in-flight
    (setq +gptel-openrouter--refresh-in-flight t)
    (require 'url)
    (let ((url-mime-accept-string "application/json")
          (url-request-method "GET")
          (url-request-extra-headers '(("Accept" . "application/json"))))
      (url-retrieve
       +gptel-openrouter-models-url
       (lambda (status)
         (unwind-protect
             (condition-case err
                 (if-let* ((error-data (plist-get status :error)))
                     (message "OpenRouter model metadata refresh failed: %S"
                              error-data)
                   (let ((http-status
                          (and (boundp 'url-http-response-status)
                               url-http-response-status)))
                     (if (and http-status
                              (not (memq http-status '(200 100))))
                         (message "OpenRouter model metadata refresh failed: HTTP %s"
                                  http-status)
                       (let* ((json (+gptel-openrouter--response-body))
                              (payload (+gptel-openrouter--parse-json json))
                              (updated (+gptel-openrouter--apply-model-metadata
                                        payload)))
                         (+gptel-openrouter--write-cache json)
                         (message "OpenRouter model metadata refreshed%s"
                                  (if updated
                                      (format " for %d gptel models" updated)
                                    ""))))))
               (error
                (message "OpenRouter model metadata refresh failed: %s"
                         (error-message-string err))))
           (setq +gptel-openrouter--refresh-in-flight nil)
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer)))))
       nil t t))))

(defun +gptel-openrouter-refresh-model-metadata (&optional force)
  "Refresh OpenRouter model metadata for gptel.

With FORCE, fetch from OpenRouter even when the cache is fresh."
  (interactive "P")
  (let ((cache-fresh (+gptel-openrouter--cache-fresh-p))
        (cached-payload
         (condition-case err
             (+gptel-openrouter--read-cache)
           (error
            (message "OpenRouter model metadata cache read failed: %s"
                     (error-message-string err))
            nil))))
    (when cached-payload
      (+gptel-openrouter--apply-model-metadata cached-payload))
    (if (and cache-fresh cached-payload (not force))
        (when (called-interactively-p 'interactive)
          (message "OpenRouter model metadata cache is fresh"))
      (+gptel-openrouter--fetch-model-metadata))))
