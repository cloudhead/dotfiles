#!/bin/sh

if command -v keychain >/dev/null; then
  eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa ~/.radicle/keys/radicle)
fi

export PATH=~/bin:~/.local/bin:~/.yarn/bin:~/.gcloud/bin:~/.cabal/bin:~/.radicle/bin:~/.cargo/bin:~/.gem/ruby/2.6.0/bin:~/.npm-packages/bin:$PATH
export PATH=$PATH:~/.foundry/bin
export PATH=$PATH:~/.local/share/gem/ruby/3.0.0/bin
export NODE_PATH=/usr/lib/node_modules
export EDITOR=nvim
export VISUAL=nvim
export MOZ_USE_XINPUT2=1 # Pixel scrolling in Firefox
export RIPGREP_CONFIG_PATH=$HOME/.rgrc
export NNN_FIFO=/tmp/nnn.fifo
export CARGO_BUILD_JOBS=12

systemctl --user start share-sync

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer
