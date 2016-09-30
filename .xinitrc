#!/bin/sh
#
# ~/.xinitrc
#
xset r rate 180 40                   # Sane repeat rate
xset -b                              # No bell
xset -dpms                           # Keep screen on at all times
xset s off                           #
xset m 8/5 0                         # Pointer settings
setxkbmap -option compose:ralt       # Set compose key to R-Alt

[ -f ~/.profile ]   && . ~/.profile
[ -f ~/.synaptics ] && . ~/.synaptics

if [ -f ~/.Xmodmap ]; then
  xmodmap ~/.Xmodmap;
fi

if [ -f ~/.Xresources ]; then
  xrdb -merge ~/.Xresources
fi

compton -c -b -r 24 --active-opacity   0.95 \
                    --inactive-opacity 0.85 \
                    --vsync opengl
exec xmonad
