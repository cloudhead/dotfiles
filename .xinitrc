#!/bin/sh
#
# ~/.xinitrc
#
set -uo pipefail

. ~/bin/toggle-displays

[ -f ~/.synaptics ] && . ~/.synaptics

setxkbmap -option compose:lwin
setxkbmap -option compose:rwin
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl

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

# `dbus-launch` is required for flameshot.
exec dbus-launch xmonad
