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
