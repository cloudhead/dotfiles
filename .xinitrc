#!/bin/sh
#
# ~/.xinitrc
#
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

if [ -f /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/stop_timeout ]; then
	echo 300s | sudo tee /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/stop_timeout
fi

udiskie --no-notify --use-udisks2 &

# Auto-lock
# xautolock -time 5 -locker slock &

[ -f ~/.fehbg ] && . ~/.fehbg
[ -f ~/.wacom ] && . ~/.wacom

exec xmonad
