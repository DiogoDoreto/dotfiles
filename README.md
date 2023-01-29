# My dotfiles management

Initial clone:

```sh
git clone --bare git@github.com:DiogoDoreto/dotfiles.git $HOME/.dotfiles
git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME checkout
```

Inspired by https://www.atlassian.com/git/tutorials/dotfiles
