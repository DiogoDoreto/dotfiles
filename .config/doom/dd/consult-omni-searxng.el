;;; dd/consult-omni-searxng.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Consult-omni source for the local SearxNG instance.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-searxng-url
  "https://search.local.doreto.com.br/search"
  "SearxNG search endpoint."
  :group 'consult-omni
  :type 'string)

(defvar consult-omni-searxng-search-url consult-omni-searxng-url
  "SearxNG URL used when opening a search in a browser.")

(cl-defun consult-omni--searxng-fetch-results
    (input &rest args &key callback &allow-other-keys)
  "Fetch SearxNG results for INPUT with ARGS.

CALLBACK is called with annotated candidates as results arrive."
  (pcase-let* ((`(,query . ,opts)
                (consult-omni--split-command
                 input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count))
                               (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page))
                              (string-to-number page))
                         consult-omni-default-page))
               (page (1+ page))
               (params `(("q" . ,query)
                         ("format" . "json")
                         ("count" . ,(format "%s" count))
                         ("pageno" . ,(format "%s" page))))
               (headers '(("Accept" . "application/json")
                          ("User-Agent" . "Emacs:consult-omni"))))
    (consult-omni--fetch-url
     consult-omni-searxng-url consult-omni-http-retrieve-backend
     :encoding 'utf-8
     :params params
     :headers headers
     :parser #'consult-omni--json-parse-buffer
     :callback
     (lambda (attrs)
       (when-let ((raw-results (gethash "results" attrs)))
         (let ((annotated-results
                (mapcar
                 (lambda (item)
                   (let* ((source "SearxNG")
                          (title (or (gethash "title" item) ""))
                          (url (or (gethash "url" item) ""))
                          (snippet (or (gethash "content" item) ""))
                          (search-url
                           (consult-omni--make-url-string
                            consult-omni-searxng-search-url params))
                          (decorated
                           (funcall consult-omni-default-format-candidate
                                    :source source :query query :url url
                                    :title title :snippet snippet)))
                     (propertize decorated
                                 :source source
                                 :title title
                                 :url url
                                 :search-url search-url
                                 :query query
                                 :snippet snippet)))
                 raw-results)))
           (funcall callback annotated-results)
           annotated-results))))))

(consult-omni-define-source "SearxNG"
                            :narrow-char ?s
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--searxng-fetch-results
                            :on-new (apply-partially
                                     #'consult-omni-external-search-with-engine
                                     "SearxNG")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda ()
                                       (and (stringp consult-omni-searxng-url)
                                            (not (string-empty-p
                                                  consult-omni-searxng-url))))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)
