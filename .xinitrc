#!/bin/sh
#
# ~/.xinitrc
#
xset r rate 180 40                   # Sane repeat rate
xset -b                              # No bell
xset -dpms                           # Keep screen on at all times
xset s off                           #
xset m 7/5 0                         # Pointer settings
setxkbmap -option compose:ralt       # Set compose key to R-Alt

[ -f ~/.profile ]   && . ~/.profile
[ -f ~/.synaptics ] && . ~/.synaptics

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
