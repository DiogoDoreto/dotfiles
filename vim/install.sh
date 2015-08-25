# Vundle: Vim plugin manager
git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim

# YouCompleteMe: This plugin takes the longest time to download all its deps. So, let's do the downloading as a separate step.
git clone https://github.com/Valloric/YouCompleteMe.git $HOME/.vim/bundle/YouCompleteMe
cd $HOME/.vim/bundle/YouCompleteMe && git submodule update --init --recursive \
   && ./install.py --gocode-completer

# Install and make vimproc (needed for typescript's plugin tsuquyomi)
git clone https://github.com/Shougo/vimproc.vim.git $HOME/.vim/bundle/vimproc.vim
cd $HOME/.vim/bundle/vimproc.vim && make
