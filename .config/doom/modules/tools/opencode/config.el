;;; config.el --- Functions to interact with OpenCode REST API -*- lexical-binding: t; -*-

(defcustom dd/opencode-api-hostname "http://localhost:4096"
  "Hostname and port for the opencode REST API."
  :type 'string
  :group 'dd/opencode)

(defcustom dd/opencode-provider-id "github-copilot"
  "Provider ID for opencode API."
  :type 'string
  :group 'dd/opencode)

(defcustom dd/opencode-model-id "gpt-4.1"
  "Model ID for opencode API."
  :type 'string
  :group 'dd/opencode)

(defvar dd/opencode-active-session-id nil
  "ID of the currently active opencode session.")

(require 'request)
(require 'consult)

(defun dd/opencode-post-message (message)
  "Post MESSAGE to the active session. If called interactively, use region as
default, else prompt."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Message: "))))
  (unless dd/opencode-active-session-id
    (user-error "No active session selected."))
  (let* ((part-id (format-time-string "%s"))
         (body (json-encode
                `(("providerID" . ,dd/opencode-provider-id)
                  ("modelID" . ,dd/opencode-model-id)
                  ("parts" . [ (("id" . ,part-id)
                                ("type" . "text")
                                ("text" . ,message))]))))
         (url (format "%s/session/%s/message"
                      dd/opencode-api-hostname
                      dd/opencode-active-session-id)))
    (message "Posting message to %s" dd/opencode-api-hostname)
    (request
      url
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data body
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Message posted successfully: %S" data)))
      :error (cl-function
              (lambda (&rest _ &allow-other-keys)
                (message "Failed to post message to opencode API."))))))


(defun dd/opencode-get-sessions (callback)
  "Fetch sessions from the opencode REST API and call CALLBACK with the result.
CALLBACK is called with a list of session objects."
  (request
    (concat dd/opencode-api-hostname "/session")
    :type "GET"
    :parser 'json-read
    :success (eval `(cl-function
                     (lambda (&key data &allow-other-keys)
                       (funcall ',callback data))))
    :error (cl-function
            (lambda (&rest _ &allow-other-keys)
              (message "Failed to fetch sessions from opencode API.")))))

(defun dd/opencode-select-active-session ()
  "Interactively select an active session from the opencode API and store its ID."
  (interactive)
  (dd/opencode-get-sessions
   (lambda (sessions)
     (let* ((titles (mapcar (lambda (s) (alist-get 'title s)) sessions))
            (selected-title (consult--read
                             titles
                             :prompt "Select session: "))
            (selected-session (seq-find (lambda (s)
                                          (string= (alist-get 'title s) selected-title))
                                        sessions)))
       (setq dd/opencode-active-session-id (alist-get 'id selected-session))
       (message "Selected session: %s (ID: %s)" selected-title dd/opencode-active-session-id)))))
