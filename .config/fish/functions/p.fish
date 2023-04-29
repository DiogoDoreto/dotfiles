function p
  set -l dirs ~/projects/* ~/.config/nvim ~/.config/astronvim ~/.config/fish ~/.config/wezterm ~/.config/ranger ~/.config/zk
  set -l choice (string join \n $dirs | fzf)
  if test -n "$choice"
    cd "$choice"
  end 
end

