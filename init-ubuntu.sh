#!/usr/bin/env sh

chmod -R 775 ~/.local/share/doomemacs/cache
chmod -R 775 ~/.local/share/doomemacs/profiles
chmod -R 775 ~/.local/share/doomemacs/state
~/.config/emacs/bin/doom install
systemctl --user restart emacs.service
