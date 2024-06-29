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

if xset q &>/dev/null; then
  xset r rate 180 40                   # Sane repeat rate
  xset -b                              # No bell
  xset -dpms                           # Keep screen on at all times
  xset s off                           #
  xset m 7/5 0                         # Pointer settings
  setxkbmap us -variant altgr-intl
fi

if command -v xbindkeys >/dev/null 2>&1; then
  xbindkeys
fi

# `dbus-launch` is required for flameshot.
exec dbus-launch spectrwm
