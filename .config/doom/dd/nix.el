;;; $DOOMDIR/dd/nix.el --- Nix Customizations -*- lexical-binding: t; -*-

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

;;; Code:

(require 'json)
(require 'subr-x)

(setq +lookup-provider-url-alist
      (append +lookup-provider-url-alist
              '(("Nix Packages" "https://search.nixos.org/packages?query=%s")
                ("Nix Options" "https://search.nixos.org/options?query=%s")
                ("Nix Wiki" "https://nixos.wiki/index.php?search=%s&go=Go&fulltext=1"))))

(with-eval-after-load 'nix-mode
  (defun +nix-extras/send-pkgs-to-nix-repl ()
    "Send `pkgs = import <nixpkgs> {}` to the active REPL."
    (interactive)
    (comint-send-string (get-buffer-process (current-buffer))
                        "pkgs = import <nixpkgs> {}\n"))

  (map! :map nix-repl-mode-map
        :localleader
        :desc "Send pkgs import" "p" #'+nix-extras/send-pkgs-to-nix-repl))

;;; dd-nix-version-compare

(defvar dd-nix-version-compare-host-flake "~/projects/dotfiles/hosts/lapdog"
  "Host flake used as the source of truth for pinned package versions.")

(defvar dd-nix-version-compare-host "lapdog"
  "NixOS configuration name inside `dd-nix-version-compare-host-flake'.")

(defvar dd-nix-version-compare-system "x86_64-linux"
  "System used when reading latest package metadata from upstream flakes.")

(defun dd-nix-version-compare--parse-ref (ref)
  "Parse package REF into (INPUT . ATTR-PATH)."
  (unless (string-match-p "\\`[^#]+#[^#]+\\'" ref)
    (user-error "Expected package ref like nixpkgs#claude-code"))
  (let ((parts (split-string ref "#" t)))
    (cons (car parts) (cadr parts))))

(defun dd-nix-version-compare--original-to-url (original)
  "Convert a flake lock ORIGINAL alist into a flake URL string."
  (let ((type (alist-get 'type original)))
    (pcase type
      ("github"
       (let ((owner (alist-get 'owner original))
             (repo (alist-get 'repo original))
             (ref (alist-get 'ref original)))
         (unless (and owner repo)
           (user-error "Cannot reconstruct GitHub flake URL from %S" original))
         (concat "github:" owner "/" repo (when ref (concat "/" ref)))))
      ("git"
       (let ((url (alist-get 'url original))
             (ref (alist-get 'ref original)))
         (unless url
           (user-error "Cannot reconstruct git flake URL from %S" original))
         (concat "git+" url (when ref (concat "?ref=" ref)))))
      ("tarball"
       (let ((url (alist-get 'url original)))
         (unless url
           (user-error "Cannot reconstruct tarball flake URL from %S" original))
         url))
      ("path"
       (user-error "Input resolves to a path flake, so no upstream latest version is available: %S" original))
      (_
       (user-error "Unsupported flake input type %S in %S" type original)))))

(defun dd-nix-version-compare--metadata-command ()
  "Return the command used to fetch host flake metadata as JSON."
  (list "nix" "flake" "metadata" "--json"
        (expand-file-name dd-nix-version-compare-host-flake)))

(defun dd-nix-version-compare--latest-url-from-metadata (metadata input)
  "Return the unpinned upstream flake URL for INPUT from METADATA."
  (let* ((locks (alist-get 'locks metadata))
         (nodes (alist-get 'nodes locks))
         (root-name (alist-get 'root locks))
         (root (alist-get (intern root-name) nodes))
         (root-inputs (alist-get 'inputs root))
         (node-name (alist-get (intern input) root-inputs))
         (node (and node-name (alist-get (intern node-name) nodes)))
         (original (alist-get 'original node)))
    (unless node-name
      (user-error "Input %s is not present in host flake root inputs" input))
    (unless original
      (user-error "Input %s has no original flake metadata" input))
    (dd-nix-version-compare--original-to-url original)))

(defun dd-nix-version-compare--current-expr (input attr-path)
  "Return a Nix expression for pinned INPUT ATTR-PATH version."
  (format "(builtins.getFlake %S).inputs.%s.legacyPackages.%s.%s.version"
          (expand-file-name dd-nix-version-compare-host-flake)
          input
          dd-nix-version-compare-system
          attr-path))

(defun dd-nix-version-compare--latest-expr (latest-url attr-path)
  "Return a Nix expression for unpinned ATTR-PATH version from LATEST-URL."
  (format "(builtins.getFlake %S).legacyPackages.%s.%s.version"
          latest-url
          dd-nix-version-compare-system
          attr-path))

(defun dd-nix-version-compare--latest-metadata-expr (latest-url attr-path)
  "Return a Nix expression for compact metadata about ATTR-PATH from LATEST-URL."
  (format
   "let pkg = (builtins.getFlake %S).legacyPackages.%s.%s; in { description = pkg.meta.description or \"\"; homepage = pkg.meta.homepage or \"\"; license = pkg.meta.license.spdxId or pkg.meta.license.shortName or pkg.meta.license.fullName or \"\"; mainProgram = pkg.meta.mainProgram or \"\"; broken = pkg.meta.broken or false; unfree = pkg.meta.unfree or false; }"
   latest-url
   dd-nix-version-compare-system
   attr-path))

(defun dd-nix-version-compare--read-json-output (output)
  "Read JSON from OUTPUT, ignoring Nix warnings that precede the object."
  (let ((json-start (string-match-p "{" output)))
    (unless json-start
      (user-error "No JSON object found in command output: %s" output))
    (json-read-from-string (substring output json-start))))

(defun dd-nix-version-compare--last-output-line (output)
  "Return the last non-empty line from OUTPUT."
  (car (last (split-string output "\n" t))))

(defun dd-nix-version-compare--format-metadata (metadata-output)
  "Format compact package metadata from METADATA-OUTPUT."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (metadata (dd-nix-version-compare--read-json-output metadata-output))
         (description (alist-get 'description metadata))
         (homepage (alist-get 'homepage metadata))
         (license (alist-get 'license metadata))
         (main-program (alist-get 'mainProgram metadata))
         (broken (alist-get 'broken metadata))
         (unfree (alist-get 'unfree metadata)))
    (string-join
     (list (format "Description: %s" (or description ""))
           (format "Homepage: %s" (or homepage ""))
           (format "License: %s" (or license ""))
           (format "Main program: %s" (or main-program ""))
           (format "Broken: %s" (if (eq broken t) "true" "false"))
           (format "Unfree: %s" (if (eq unfree t) "true" "false")))
     "\n")))

(defun dd-nix-version-compare--run (command callback)
  "Run COMMAND asynchronously and call CALLBACK with (EXIT-CODE OUTPUT)."
  (let ((buffer (generate-new-buffer " *dd-nix-version-compare*")))
    (make-process
     :name "dd-nix-version-compare"
     :buffer buffer
     :command command
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((exit-code (process-exit-status process))
               (output (with-current-buffer buffer
                         (string-trim (buffer-string)))))
           (kill-buffer buffer)
           (funcall callback exit-code output)))))))

(defun dd-nix-version-compare--render-result (package-ref current latest current-expr latest-expr &optional metadata)
  "Render version comparison result for PACKAGE-REF."
  (let ((buf (get-buffer-create "*Nix Package Version Compare*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Package: %s\n\n" package-ref))
      (insert (format "Current: %s\n" current))
      (insert (format "Latest:  %s\n\n" latest))
      (when metadata
        (insert metadata "\n\n"))
      (insert (format "Current expression:\n%s\n\n" current-expr))
      (insert (format "Latest expression:\n%s\n" latest-expr))
      (goto-char (point-min))
      (view-mode 1)
      (goto-address-mode 1)
      (setq buffer-read-only t))
    (message "Done")
    (pop-to-buffer buf)))

(defun dd-nix-version-compare--render-eval-result
    (package-ref current-exit current-output latest-exit latest-output metadata-exit metadata-output current-expr latest-expr)
  "Render PACKAGE-REF comparison from Nix eval process results."
  (dd-nix-version-compare--render-result
   package-ref
   (if (zerop current-exit)
       (dd-nix-version-compare--last-output-line current-output)
     (format "current eval failed: %s" current-output))
   (if (zerop latest-exit)
       (dd-nix-version-compare--last-output-line latest-output)
     (format "latest eval failed: %s" latest-output))
   current-expr
   latest-expr
   (if (zerop metadata-exit)
       (condition-case metadata-format-error
           (dd-nix-version-compare--format-metadata metadata-output)
         (error (format "Metadata: failed to parse: %s" (error-message-string metadata-format-error))))
     (format "Metadata: failed to evaluate: %s" metadata-output))))

(defun dd-nix-version-compare--start-evals (package-ref current-expr latest-expr latest-metadata-expr)
  "Start async version and metadata evals for PACKAGE-REF."
  (dd-nix-version-compare--run
   (list "nix" "eval" "--impure" "--raw" "--expr" current-expr)
   (lambda (current-exit current-output)
     (dd-nix-version-compare--run
      (list "nix" "eval" "--impure" "--raw" "--expr" latest-expr)
      (lambda (latest-exit latest-output)
        (dd-nix-version-compare--run
         (list "nix" "eval" "--impure" "--json" "--expr" latest-metadata-expr)
         (lambda (metadata-exit package-metadata-output)
           (dd-nix-version-compare--render-eval-result
            package-ref
            current-exit current-output
            latest-exit latest-output
            metadata-exit package-metadata-output
            current-expr latest-expr))))))))

(defun dd-nix-version-compare--handle-metadata (package-ref input attr-path metadata-output)
  "Parse METADATA-OUTPUT and start evals for PACKAGE-REF."
  (condition-case metadata-error
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (metadata (dd-nix-version-compare--read-json-output metadata-output))
             (latest-url (dd-nix-version-compare--latest-url-from-metadata metadata input))
             (current-expr (dd-nix-version-compare--current-expr input attr-path))
             (latest-expr (dd-nix-version-compare--latest-expr latest-url attr-path))
             (latest-metadata-expr (dd-nix-version-compare--latest-metadata-expr latest-url attr-path)))
        (message "Evaluating current and latest versions for %s..." package-ref)
        (dd-nix-version-compare--start-evals
         package-ref current-expr latest-expr latest-metadata-expr))
    (error
     (dd-nix-version-compare--render-result
      package-ref
      (format "metadata parse failed: %s" (error-message-string metadata-error))
      "not evaluated"
      "not evaluated"
      "not evaluated"))))

(defun dd-nix-compare-package-version (package-ref)
  "Compare pinned and latest versions for PACKAGE-REF.

PACKAGE-REF must have the form INPUT#ATTR-PATH, such as
`nixpkgs#claude-code' or `llm-agents#claude-agent-acp'."
  (interactive (list (read-string "Nix package ref: ")))
  (pcase-let* ((`(,input . ,attr-path) (dd-nix-version-compare--parse-ref package-ref))
               (metadata-command (dd-nix-version-compare--metadata-command)))
    (message "Reading host flake metadata for %s..." input)
    (dd-nix-version-compare--run
     metadata-command
     (lambda (metadata-exit metadata-output)
       (if (not (zerop metadata-exit))
           (dd-nix-version-compare--render-result
            package-ref
            (format "metadata command failed: %s" metadata-output)
            "not evaluated"
            (string-join metadata-command " ")
            "not evaluated")
         (dd-nix-version-compare--handle-metadata
          package-ref input attr-path metadata-output))))))

;;; dd-nix-search

(defvar-local dd-nix-search--package-alist nil
  "stores (ID . full-package-info) for the search results buffer")

(defun dd-nix--search-json-payload (query)
  "Construct the JSON payload for the Nix search API using QUERY."
  (json-encode
   `((from . 0)
     (size . 30)
     (query
      (bool
       (filter ((term (type (value . "package")
                            (_name . "filter_packages")))))
       (must ((dis_max
               (tie_breaker . 0.7)
               (queries ((multi_match
                          (type . "cross_fields")
                          (query . ,query)
                          (analyzer . "whitespace")
                          (auto_generate_synonyms_phrase_query . :false)
                          (operator . "and")
                          (_name . "multi_match")
                          (fields . ("package_attr_name^9"
                                     "package_attr_name.*^5.3999999999999995"
                                     "package_programs^9"
                                     "package_programs.*^5.3999999999999995"
                                     "package_pname^6"
                                     "package_pname.*^3.5999999999999996"
                                     "package_description^1.3"
                                     "package_description.*^0.78"
                                     "package_longDescription^1"
                                     "package_longDescription.*^0.6"
                                     "flake_name^0.5"
                                     "flake_name.*^0.3"))))
                        ((wildcard
                          (package_attr_name
                           (value . ,(concat "*" query "*"))
                           (case_insensitive . t)))))))))))))

(defun dd-nix-search (query)
  "Search for Nix packages via https://search.nixos.org using QUERY."
  (interactive (list (read-string "Nix search query: ")))
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Authorization" . "Basic YVdWU0FMWHBadjpYOGdQSG56TDUyd0ZFZWt1eHNmUTljU2g=")))
         (url-request-data (dd-nix--search-json-payload query))
         (api-url "https://search.nixos.org/backend/latest-44-nixos-unstable/_search")
         (buf (generate-new-buffer "*Nix Package Search*")))
    (message "Searching Nix packages for: %s..." query)
    (url-retrieve
     api-url
     (lambda (_status)
       (goto-char (point-min))
       (re-search-forward "\n\n" nil t) ; skip headers
       (let* ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (data (json-read)))
         (let* ((hits (alist-get 'hits (alist-get 'hits data)))
                (package-alist
                 (mapcar (lambda (hit)
                           (let* ((src (alist-get '_source hit))
                                  (attr-name (or (alist-get 'package_attr_name src) "")))
                             (cons attr-name src)))
                         hits))
                (table
                 (mapcar
                  (lambda (hit)
                    (let* ((src (alist-get '_source hit))
                           (attr-name (or (alist-get 'package_attr_name src) ""))
                           (version (or (alist-get 'package_pversion src) ""))
                           (desc (or (alist-get 'package_description src) ""))
                           (programs (string-join (or (alist-get 'package_programs src) '()) ", "))
                           (platforms (string-join (or (alist-get 'package_platforms src) '()) ", ")))
                      (list attr-name
                            (vector attr-name version desc programs platforms))))
                  hits)))
           (with-current-buffer buf
             (setq buffer-read-only nil)
             (erase-buffer)
             (dd-nix-search-results-mode)
             (setq dd-nix-search--package-alist package-alist)
             (setq tabulated-list-entries table)
             (tabulated-list-init-header)
             (tabulated-list-print)
             (goto-char (point-min))
             (setq buffer-read-only t)
             (pop-to-buffer buf)))))
     nil t)))

(defun dd-nix-search--show-details ()
  "Show full information about the currently selected Nix package."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (pkg-src (when (boundp 'dd-nix-search--package-alist)
                    (cdr (assoc id dd-nix-search--package-alist)))))
    (if pkg-src
        (let ((buf (generate-new-buffer (format "*Nix Package: %s*" (or (alist-get 'package_attr_name pkg-src) id)))))
          (with-current-buffer buf
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert (format "Nix Package details for: %s\n\n" (or (alist-get 'package_attr_name pkg-src) id)))
            (dolist (field pkg-src)
              (let ((k (car field))
                    (v (cdr field)))
                (when v
                  (insert (format "%20s: %s\n"
                                  (symbol-name k)
                                  (if (and (listp v) (not (stringp v)))
                                      (mapconcat (lambda (item) (prin1-to-string item t)) v ", ")
                                    (prin1-to-string v t)))))))
            (goto-char (point-min))
            (view-mode 1)
            (goto-address-mode 1)
            (setq buffer-read-only t)
            (pop-to-buffer buf)))
      (message "No details available for this package."))))

(define-derived-mode dd-nix-search-results-mode tabulated-list-mode "Nix Search Results"
  "Major mode for displaying Nix package search results."
  (setq tabulated-list-format [("Name" 25 t)
                               ("Version" 10 t)
                               ("Description" 35 t)
                               ("Programs" 20 t)
                               ("Platforms" 25 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (define-key dd-nix-search-results-mode-map (kbd "RET") #'dd-nix-search--show-details)
  (evil-local-set-key 'normal (kbd "RET") #'dd-nix-search--show-details))
