#!/bin/sh
#
# ~/.xinitrc
#
exec >~/.local/share/xorg/xinitrc-$DISPLAY.log 2>&1
set -uex

[ -f ~/.synaptics ] && . ~/.synaptics

setxkbmap -option compose:lwin
setxkbmap -option compose:rwin
setxkbmap -option compose:menu
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
exec dbus-launch spectrwm
