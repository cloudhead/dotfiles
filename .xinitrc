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

while :; do
  eth=$(ip addr show eth0 | grep 'inet\s' | awk '{ print $2 }')
  acpi=$(acpi -b | awk '{ print $3, $4 }' | tr -d ',')

  iw=$(iw dev wlan0 link)
  wlan=$(echo "$iw" | grep SSID   | sed 's/SSID: //')
  qual=$(echo "$iw" | grep signal | sed 's/signal: //' )

  xsetroot -name "$eth $wlan $qual ($acpi) $(date +"%R, %D")"

  sleep 10s
done &

exec dwm
