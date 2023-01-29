function dotfiles --description 'git wrapper for dotfiles management'
  git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $argv
end

