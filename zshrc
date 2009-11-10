#!/bin/zsh
##
# cloudhead - .zshrc
#

source ~/.profile

#
# Includes
#
autoload colors && colors
autoload -Uz vcs_info  
autoload -U compinit && compinit

# Set input mode to vi
set -o vi

# Bind <C-r> to history search
bindkey "^r" history-incremental-search-backward

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
alias src="cd $SRCPATH"

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
ACTION="%{$PURPLE%}:%a%{$CLEAR%}"

zstyle ':vcs_info:*'          enable git
zstyle ':vcs_info:*'          check-for-changes true
zstyle ':vcs_info:*:clean:*'  actionformats "(%{$BLUE%}%b${ACTION})%{$CLEAR%} "
zstyle ':vcs_info:*:dirty:*'  actionformats "(%{$GREEN%}%b${ACTION})%{$CLEAR%} "
zstyle ':vcs_info:*:staged:*' actionformats "(%{$YELLOW%}%b%${ACTION})%{$CLEAR%} "
zstyle ':vcs_info:*:clean:*'  formats       "%{$BLUE%}(%b)%{$CLEAR%} "
zstyle ':vcs_info:*:dirty:*'  formats       "%{$GREEN%}(%b)%{$CLEAR%} "
zstyle ':vcs_info:*:staged:*' formats       "%{$YELLOW%}(%b)%{$CLEAR%} "
zstyle ':vcs_info:*'          nvcsformats   ""

function precmd {
  # Check if there are unstaged changes
  git status &>/dev/null

  if [ $? -eq 0 ]; then
    vcs_info 'staged'
  elif git diff --quiet 2>/dev/null >&2
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
RPROMPT='%{$PURPLE%}%w %T%{$CLEAR%} %{$CYAN%}@ %m%{$CLEAR%} %{$BLUE%}: %!%{$CLEAR%}' 

