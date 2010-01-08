#!/bin/sh
# $Xorg: xinitrc.cpp,v 1.3 2000/08/17 19:54:30 cpqbld Exp $

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11/lib/X11/xinit/.Xmodmap

xmodmap -e "keycode 59 = BackSpace"

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    if [ -x /usr/bin/cpp ] ; then
        xrdb -merge $sysresources
    else
        xrdb -nocpp -merge $sysresources
    fi
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    if [ -x /usr/bin/cpp ] ; then
        xrdb -merge "$userresources"
    else
        xrdb -nocpp -merge "$userresources"
    fi
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

source ~/.profile

# start some nice programs
if [ -d /usr/X11/lib/X11/xinit/xinitrc.d ] ; then
  for f in /usr/X11/lib/X11/xinit/xinitrc.d/*.sh ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

exec xmonad
