#!/bin/sh
##
# cloudhead .profile
#

#
# ENV
#
export PATH=/usr/local/git/bin:/usr/local/bin:/usr/local/sbin:$PATH
export CLICOLOR="true"
export LSCOLORS="Gxfxcxdxbxegedabagacad"
export EDITOR="vim"
export USERWM=`which dwm`
export SRCPATH="~/src"
export LANG="en_US.UTF-8"

if [[ -z "$DISPLAY" ]] && [[ ! -a "/tmp/.X11-unix/X0" ]]; then
  startx
  logout
fi
