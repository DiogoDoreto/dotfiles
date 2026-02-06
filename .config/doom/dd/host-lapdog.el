;;; $DOOMDIR/dd/host-lapdog.el --- lapdog host customizations -*- lexical-binding: t; -*-

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

(defun dd--make-nix (command)
  (let ((default-directory "~/projects/dotfiles/hosts/lapdog/"))
    (compile command)))

(map! :leader :prefix ("C-SPC" . "lapdog host")
      :leader :prefix ("C-SPC n" . " 󱄅 Nix")
      :desc "rebuild boot"        :n "b" (cmd! (dd--make-nix "make nixos-lapdog-boot"))
      :desc "rebuild switch"      :n "s" (cmd! (dd--make-nix "make nixos-lapdog-switch"))
      :desc "home-manager switch" :n "h" (cmd! (dd--make-nix "make hm-dog-lapdog-switch"))
      :leader :prefix ("C-SPC s" . "  SSH")
      :desc "Chungus dotfiles" :n "c" (cmd! (find-file "/sshx:chungus:p/dotfiles/")))

(defconst systemd-bus :system)
(defconst systemd-service "org.freedesktop.systemd1")
(defconst systemd-path "/org/freedesktop/systemd1")
(defconst systemd-interface "org.freedesktop.systemd1.Manager")

(defun systemd-start-unit (unit-name)
  "Start a systemd unit (e.g., nginx.service)."
  (shell-command (format "systemctl start %s" unit-name)))

(defun systemd-stop-unit (unit-name)
  "Stop a systemd unit."
  (shell-command (format "systemctl stop %s" unit-name)))

(defun systemd-restart-unit (unit-name)
  "Restart a systemd unit."
  (shell-command (format "systemctl restart %s" unit-name)))

(defun systemd-get-unit-path (unit-name)
  "Get unit path, loading it if necessary."
  (condition-case nil
      (dbus-call-method systemd-bus
                        systemd-service
                        systemd-path
                        systemd-interface
                        "GetUnit"
                        unit-name)
    (dbus-error
     ;; Fall back to LoadUnit
     (dbus-call-method systemd-bus
                       systemd-service
                       systemd-path
                       systemd-interface
                       "LoadUnit"
                       unit-name))))

;; (systemd-get-unit-path (car systemd-transient-units))

(defun systemd-unit-get-state (unit-name)
  "Get the active state of a unit (active, inactive, failed, etc.)."
  (dbus-get-property systemd-bus
                     systemd-service
                     (systemd-get-unit-path unit-name)
                     "org.freedesktop.systemd1.Unit"
                     "ActiveState"))

(defvar systemd-transient-units
  '("openvpn-vpn-es.service"
    "openvpn-vpn-br.service")
  "List of systemd units to control.")

(defvar systemd-transient-current-unit (car systemd-transient-units)
  "Currently selected unit.")

(transient-define-prefix systemd-multi-unit-menu ()
  "Control multiple systemd units."
  [:description
   (lambda ()
     (let* ((state (systemd-unit-get-state systemd-transient-current-unit))
            (state-face (pcase state
                          ("active" 'success)
                          ("inactive" 'warning)
                          ("failed" 'error)
                          (_ 'default)))
            (state-indicator (pcase state
                               ("active" "●")
                               ("inactive" "○")
                               ("failed" "✗")
                               (_ "?"))))
       (format "%s %s: %s"
               (propertize state-indicator 'face state-face)
               (propertize systemd-transient-current-unit 'face 'bold)
               (propertize state 'face state-face))))
   ["Actions"
    ("s" "Start"
     (lambda () (interactive)
       (systemd-start-unit systemd-transient-current-unit))
     :if-not (lambda () (equal (systemd-unit-get-state systemd-transient-current-unit) "active")))
    ("S" "Stop"
     (lambda () (interactive)
       (systemd-stop-unit systemd-transient-current-unit))
     :if (lambda () (equal (systemd-unit-get-state systemd-transient-current-unit) "active")))
    ("r" "Restart"
     (lambda () (interactive)
       (systemd-restart-unit systemd-transient-current-unit))
     :if (lambda () (equal (systemd-unit-get-state systemd-transient-current-unit) "active")))]
   ["Unit Selection"
    ("n" "Next unit"
     (lambda ()
       (interactive)
       (let* ((units systemd-transient-units)
              (current systemd-transient-current-unit)
              (idx (cl-position current units :test 'equal))
              (next-idx (mod (1+ idx) (length units))))
         (setq systemd-transient-current-unit (nth next-idx units))
         (transient-setup 'systemd-multi-unit-menu))))
    ("u" "Choose unit"
     (lambda ()
       (interactive)
       (setq systemd-transient-current-unit
             (completing-read "Unit: " systemd-transient-units))
       (transient-setup 'systemd-multi-unit-menu)))]
   ["Other"
    ("g" "Refresh" (lambda () (interactive) (transient-setup 'systemd-multi-unit-menu)))
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (require 'dbus)
  (transient-setup 'systemd-multi-unit-menu))
