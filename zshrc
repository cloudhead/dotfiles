#!/bin/zsh
##
# cloudhead - .zshrc
#

#
# Includes
#
autoload colors && colors
autoload -Uz vcs_info  
autoload -U compinit && compinit

#
# ENV
#
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
export PATH="/opt/local/bin:/opt/local/sbin:${PATH}"
export PATH="/usr/local/mysql/bin:/usr/local/git/bin:/usr:${PATH}"
export MANPATH="/opt/local/share/man:${MANPATH}"
export CLICOLOR="true"
export LSCOLORS="gxfxcxdxbxegedabagacad"
export EDITOR="vim"

#
# Aliases
#

# git
alias gush='git push'
alias github='gush origin master'
alias origin='gush origin master'
alias gist='git status'
alias gull='git pull'
alias gadd='git add'
alias gim='git commit -m'

alias ls='ls -alAGhp'
alias ..='cd ..'

# other
alias nginxre='sudo kill `cat /usr/local/logs/nginx.pid`;sudo nginx'
alias ts='thin start'

#
# History
#
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
REPORTTIME=10
 
setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS
setopt LOCAL_TRAPS
setopt EXTENDED_HISTORY
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt IGNORE_EOF
setopt AUTO_CD
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt HIST_REDUCE_BLANKS

#
# ANSI Color Codes
#
CLEAR=$reset_color
RED=$fg[red]
GREEN=$fg[green]
YELLOW=$fg[yellow]
BLUE=$fg[blue]
PURPLE=$fg[magenta]
CYAN=$fg[cyan]

#
# Prompt
#
function prompt_cursor {
  STATUS=$?
  
  local prompt=""
  
  # Status
  if [[ $STATUS == 0 ]]; then
    prompt=$GREEN
  elif [[ $STATUS == 1 ]]; then
    prompt=$YELLOW
  else
    prompt=$RED
  fi
  
  echo "%{$prompt%}%#%{$CLEAR%}"
}

#
# Set prompt style
#
ACTION="%{$CYAN%}%a%{$CLEAR%}"

zstyle ':vcs_info:*'         enable git
zstyle ':vcs_info:*'         check-for-changes true
zstyle ':vcs_info:*'         unstagedstr   "%{$GREEN%}+%{$CLEAR%} "
zstyle ':vcs_info:*'         stagedstr     "%{$YELLOW%}!%{$CLEAR%} "
zstyle ':vcs_info:*:clean:*' actionformats "%{$BLUE%}(%b:${ACTION})%{$CLEAR%} "
zstyle ':vcs_info:*:dirty:*' actionformats "%{$GREEN%}(%b):${ACTION}%{$CLEAR%} "
zstyle ':vcs_info:*:clean:*' formats       "%{$BLUE%}(%b)%{$CLEAR%} "
zstyle ':vcs_info:*:dirty:*' formats       "%{$GREEN%}(%b)%{$CLEAR%} "
zstyle ':vcs_info:*'         nvcsformats   ""

function precmd {
  # Check if there are unstaged changes 
  if git diff --quiet 2>/dev/null >&2 
  then
    vcs_info 'clean'
  else
    vcs_info 'dirty'
  fi
}

#
# Set Prompts
#
PROMPT='%{$CYAN%}%n%{$CLEAR%} %~ $vcs_info_msg_0_$(prompt_cursor) %{$CLEAR%}'
RPROMPT='%{$PURPLE%}%w %T%{$CLEAR%} %{$CYAN%}@%m%{$CLEAR%} %{$BLUE%}: %!%{$CLEAR%}' 
