#!/bin/sh
#
# ~/.xinitrc
#
xset r rate 180 40                   # Sane repeat rate
xset -b                              # No bell
xset -dpms                           # Keep screen on at all times
xset s off                           #
xset m 4/3 0                         # Pointer settings
setxkbmap -option compose:ralt       # Set compose key to R-Alt

[ -f ~/.profile ]   && . ~/.profile
[ -f ~/.synaptics ] && . ~/.synaptics

if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap;
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

exec dwm
