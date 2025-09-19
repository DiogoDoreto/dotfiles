;;; $DOOMDIR/dd/elisp.el --- emacs-lisp customizations -*- lexical-binding: t; -*-

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

(map! :leader :desc "Open IELM" :n "o i" #'ielm)

(map! :localleader
      :map (emacs-lisp-mode-map lisp-interaction-mode-map)
      (:prefix ("d" . "debug")
               "t" #'trace-function
               "T" #'trace-function-background
               "u" #'untrace-function
               "U" #'untrace-all))
