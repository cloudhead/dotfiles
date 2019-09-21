#!/bin/sh
#
# ~/.xinitrc
#
set -uo pipefail

. ~/bin/toggle-displays

[ -f ~/.profile ]   && . ~/.profile
[ -f ~/.synaptics ] && . ~/.synaptics

setxkbmap -option compose:rwin

if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap;
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

udiskie --no-notify --use-udisks2 &

[ -f ~/.fehbg ] && . ~/.fehbg
[ -f ~/.wacom ] && . ~/.wacom

exec xmonad
