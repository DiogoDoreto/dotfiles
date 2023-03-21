function df_lazy --description 'open dotfiles in lazygit'
  lazygit --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $argv
end

