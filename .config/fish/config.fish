if status is-interactive
    # Commands to run in interactive sessions can go here

    # Prevents macos from eating CTRL-V
    # see https://github.com/kevinhwang91/rnvimr/issues/71
    # see https://github.com/jonas/tig/issues/314#issuecomment-57881920
    stty lnext undef
end
