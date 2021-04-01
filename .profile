#!/bin/sh

if command -v keychain >/dev/null; then
  eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)
fi

export PATH=~/bin:~/.local/bin:~/.cabal/bin:~/.cargo/bin:~/.gem/ruby/2.6.0/bin:~/.radicle/bin:~/.npm-packages/bin:$PATH
export EDITOR=nvim
export VISUAL=nvim
export MOZ_USE_XINPUT2=1 # Pixel scrolling in Firefox
export RIPGREP_CONFIG_PATH=$HOME/.rgrc
export PATH="$HOME/.radicle/bin:$PATH"

systemctl --user start share-sync

if command -v xinput >/dev/null; then
  prop="$(xinput | rg Touchpad | rg -o 'id=[0-9]+' | sed 's/id=//')"
  xinput --set-prop "$prop" "libinput Disable While Typing Enabled" 0
fi

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer
