#!/usr/bin/env bash

cp .vimrc ~/.vimrc
cp .zshrc ~/.zshrc
cp .zshenv ~/.zshenv
cp .tmux.conf ~/tmux.conf
cp .gitconfig ~/.gitconfig
cp .gitignore ~/.gitignore
cp .dircolors ~/.dircolors
cp -R .vim ~/.vim
cp -r .config/nvim/ ~/.config/nvim
cp -r .config/coc/ ~/.config/coc

./install-eth-toolchain.sh
