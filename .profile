#!/bin/sh

eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)
export PATH=~/bin:~/.local/bin:~/.cabal/bin:~/.cargo/bin:$PATH
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/cuda/lib64:/opt/cuda/extras/CUPTI/lib64"
export CUDA_HOME=/opt/cuda
export EDITOR=nvim
export VISUAL=nvim
export MOZ_USE_XINPUT2=1 # Pixel scrolling in Firefox
xinput --set-prop "11" "libinput Disable While Typing Enabled" 0
