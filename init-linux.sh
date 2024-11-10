#!/usr/bin/env sh

# init git
read -p "git config user.email " email
git config --file ~/.config/git/config.private user.email $email

# init doom emacs
mkdir -p ~/org/roam
chmod -R 775 ~/.local/share/doomemacs/cache
chmod -R 775 ~/.local/share/doomemacs/profiles
chmod -R 775 ~/.local/share/doomemacs/state
~/.config/emacs/bin/doom install
systemctl --user restart emacs.service
