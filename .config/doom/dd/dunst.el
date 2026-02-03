;;; $DOOMDIR/dd/dunst.el --- integrate with dunst notifications -*- lexical-binding: t; -*-

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

(defun dd--system-uptime ()
  "Get system uptime in seconds from /proc/uptime."
  (with-temp-buffer
    (insert-file-contents "/proc/uptime")
    (string-to-number (car (split-string (buffer-string))))))

(defun dd--dunst-ts-to-time (ts)
  "Convert dunst timestamp TS (in nanoseconds) to Unix timestamp."
  (let* ((dunst-ts (/ ts 1000000))  ; Convert ns to seconds
         (uptime (dd--system-uptime))
         (ts-now (float-time))
         (unix-ts (+ (- ts-now uptime) dunst-ts)))
    unix-ts))

(defun dd/open-notifications ()
  "Display dunst notification history in an org-mode buffer."
  (interactive)
  (require 'json)
  (let* ((json-output (shell-command-to-string "dunstctl history"))
         (json-data (json-read-from-string json-output))
         (notifications (append (aref (cdr (assoc 'data json-data)) 0) nil)))
    (with-current-buffer (get-buffer-create "*notifications*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Notification History\n\n")
        (if (zerop (length notifications))
            (insert "No notifications in history.\n")
          (mapc #'dd--format-notification (append notifications nil)))
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer))
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'kill-current-buffer))))

(defun dd--format-notification (notification)
  "Format a single NOTIFICATION as an org entry."
  (let-alist notification
    (insert (format "* %s\n" (or .summary.data "No summary")))
    (insert (format "  :PROPERTIES:\n"))
    (insert (format "  :APP: %s\n" (or .appname.data "Unknown")))
    (insert (format "  :TIMESTAMP: %s\n"
                    (if .timestamp.data
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (dd--dunst-ts-to-time .timestamp.data))
                      "N/A")))
    (when .urgency.data
      (insert (format "  :URGENCY: %s\n" .urgency.data)))
    (insert (format "  :END:\n"))
    (when (and .body.data (not (string-empty-p .body.data)))
      (insert (format "\n%s\n" .body.data)))
    (insert "\n")))

(map! :leader :desc "Open Notifications" :n "o n" #'dd/open-notifications)
