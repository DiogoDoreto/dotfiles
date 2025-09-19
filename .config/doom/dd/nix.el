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

(setq +lookup-provider-url-alist
      (append +lookup-provider-url-alist
              '(("Nix Packages" "https://search.nixos.org/packages?query=%s")
                ("Nix Options" "https://search.nixos.org/options?query=%s")
                ("Nix Wiki" "https://nixos.wiki/index.php?search=%s&go=Go&fulltext=1"))))

(after! nix-mode
  (defun +nix-extras/send-pkgs-to-nix-repl ()
    "Send `pkgs = import <nixpkgs> {}` to the active REPL."
    (interactive)
    (comint-send-string (get-buffer-process (current-buffer))
                        "pkgs = import <nixpkgs> {}\n"))

  (map! :map nix-repl-mode-map
        :localleader
        :desc "Send pkgs import" "p" #'+nix-extras/send-pkgs-to-nix-repl))

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

