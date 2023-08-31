if status is-interactive
  abbr --add e nvim
  abbr --add g lazygit
  abbr --add r ranger
  abbr --add ll exa --icons --long
  abbr --add la exa --icons --long --all
  abbr --add lld exa --icons --long --only-dirs

  function multicd
    echo cd (string repeat -n (math (string length -- $argv[1]) - 1) ../)
  end
  abbr --add dotdot --regex '^\.\.+$' --function multicd
end
