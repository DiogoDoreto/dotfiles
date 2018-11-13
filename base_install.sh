#!/usr/bin/env bash

sudo apt update
sudo apt install -y \
  build-essential \
  git \
  unrar \
  unzip \
  vim \
  wget \
  zsh \
  --no-install-recommends

chsh -s $(which zsh)
