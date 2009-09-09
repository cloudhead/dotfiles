##
# cloudhead - .profile
#

#
# Includes
#
. /usr/local/etc/git-completion.bash

#
# ENV
#
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export PATH=/usr/local/mysql/bin:/usr/local/git/bin:/usr:$PATH
export MANPATH=/opt/local/share/man:$MANPATH
export CLASSPATH=$HOME/Sites/clojure/*:/usr/local/jar/*
export CLICOLOR="true"
export LSCOLORS="gxfxcxdxbxegedabagacad"

#
# Aliases
#

# git
alias gush='git push'
alias github='gush origin master'
alias gist='git status'
alias gull='git pull'
alias gadd='git add'

# ls & cd
if [[ $SYSTEM =~ $SYSTEM_NIX ]]; then
  alias ls='ls --color=always -alAFhp'
else
  alias ls='ls -alAGhp'
fi

alias ..='cd ..'

# other
alias nginxre='sudo kill `cat /usr/local/logs/nginx.pid`;sudo nginx'

#
# ANSI Codes
#
CLEAR=$'\[\e[39m\]'
BLACK=$'\e[30m'
RED=$'\e[31m'
GREEN=$'\e[32m'
YELLOW=$'\e[33m'
BLUE=$'\e[34m'
PURPLE=$'\e[35m'
CYAN=$'\[\e[36m\]'
WHITE=$'\e[37m'

#
# Prompt
#
function prompt {
  STATUS=$?

  # Branch
  if git diff --quiet 2>/dev/null >&2 
  then
    PS1=$GREEN
  else
    PS1=$YELLOW
  fi
  
  PS1=$PS1$(__git_ps1)
  
  # Status
  if [[ $STATUS == 0 ]]; then
    PS1=$PS1$GREEN
  elif [[ $STATUS == 1 ]]; then
    PS1=$PS1$YELLOW
  else
    PS1=$PS1$RED
  fi
  
  echo $PS1
}

export PS1="$YELLOW\t $CYAN\u@\h$CLEAR \w\$(prompt)\n\$$CLEAR "

