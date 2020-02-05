#!/bin/sh
#
# ~/.xinitrc
#
set -uo pipefail

. ~/bin/toggle-displays

[ -f ~/.profile ]   && . ~/.profile
[ -f ~/.synaptics ] && . ~/.synaptics

setxkbmap -option compose:lwin
setxkbmap -option compose:rwin

if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap;
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

if command -v udiskie >/dev/null 2>&1; then
  udiskie --no-notify --use-udisks2 &
fi

[ -f ~/.fehbg ] && . ~/.fehbg
[ -f ~/.wacom ] && . ~/.wacom

exec xmonad
