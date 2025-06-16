;;; devdocs.el --- gptel devdocs tools -*- lexical-binding: t; -*-
;;
;;; Code:
(require 'devdocs-browser)

(defun dd/devdocs-browser--entries-as-json-string (data)
  (let* ((entries (plist-get (plist-get data :index) :entries))
         (jsonlist (mapcar (lambda (entry)
                             (concat "{ \"key\": \"" (plist-get entry :path) "\", \"description\": \"" (plist-get entry :name) "\" }"))
                           entries)))
    (concat "[\n" (string-join jsonlist ",\n") "\n]")))

(defun dd/devdocs-browser--get-contents (doc url-path)
  "Return the contents at the documentation URL given DOC and URL-PATH.
If the documentation is installed offline, read the local file;
otherwise, download and return the contents from the online URL."
  ;; mostly copied the implementation from `devdocs-browser--eww-open'
  (let* ((slug (plist-get doc :slug))
         (mtime (plist-get doc :mtime))
         (path-and-query (string-split url-path "#"))
         (path (car path-and-query))
         (query (cadr path-and-query))
         (offline-data-dir (devdocs-browser-offline-data-dir slug))
         (url (if offline-data-dir
                  (concat (convert-standard-filename
                           (expand-file-name (concat "./" path ".html") offline-data-dir))
                          (when query (concat "#" query)))
                (concat devdocs-browser-doc-base-url slug "/"
                        path
                        (format ".html?%s" mtime)
                        (when query (concat "#" query))))))
    (let ((offline-data-dir (devdocs-browser-offline-data-dir slug)))
      (if offline-data-dir
          (setq url (concat "file://"
                            (when (memq system-type '(windows-nt ms-dos)) "/")
                            (convert-standard-filename
                             (expand-file-name (concat "./" path ".html") offline-data-dir))
                            (when query (concat "#" query))))
        (setq url (concat devdocs-browser-doc-base-url slug "/"
                          path
                          (format ".html?%s" mtime)
                          (when query (concat "#" query))))))
    (cond
     (offline-data-dir (with-temp-buffer
                         (insert-file-contents url)
                         (buffer-string)))
     ((not offline-data-dir)
      (require 'url)
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (buffer-substring-no-properties (point) (point-max))))
     (t
      (error "Unsupported URL scheme")))))

(gptel-make-tool
 :name "retrieve_documentation"
 :description "Searches documentation on a topic"
 :category "docs"
 :function (lambda (topic path)
             (dd/devdocs-browser--get-contents
              (devdocs-browser--load-doc topic)
              path))
 :args `((:name "topic"
          :type string
          :enum ,(vconcat (devdocs-browser-list-installed-slugs))
          :description "A general topic for the search")
         (:name "key"
          :type string
          :description "Key for the documentation page. MUST MATCH EXACTLY one of the `key` values from the output of the `retrieve_documentation_keys` tool.")))

(gptel-make-tool
 :name "retrieve_documentation_keys"
 :description "Returns a JSON list of available keys to be used as a parameter of `retrieve_documentation` tool"
 :category "docs"
 :function #'dd--docs-entries-as-json-string
 :args `((:name "topic"
          :type string
          :enum ,(vconcat (devdocs-browser-list-installed-slugs))
          :description "A general topic for the search")))
