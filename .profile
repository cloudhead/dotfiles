#!/bin/sh

eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)
export PATH=~/bin:~/.local/bin:~/.cabal/bin:~/.cargo/bin:~/.gem/ruby/2.6.0/bin:~/.npm-packages/bin:$PATH
export EDITOR=nvim
export VISUAL=nvim
export MOZ_USE_XINPUT2=1 # Pixel scrolling in Firefox

if command -v xinput >/dev/null; then
	xinput --set-prop "15" "libinput Disable While Typing Enabled" 0
fi
