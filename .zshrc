##
# cloudhead - .zshrc
#
export PATH=~/bin:~/.cabal/bin:$PATH

source ~/.profile
source ~/.awsrc

#
# Includes
#
autoload colors && colors
autoload -U compinit && compinit
autoload -U complist

# Set input mode to vi
set -o vi

# Bind <C-r> to history search
bindkey "^r" history-incremental-search-backward

#
# Aliases
#
alias g='git'
alias ls='ls -p'
alias l='ls -lFGh'
alias ll='ls -lFAGh'
alias mv='/bin/mv -i'
alias ..='cd ..'
alias img='sxiv'
alias df='df -h'
alias sys='systemctl'
alias x='startx'
alias web='chromium'
alias e='vim'
alias pdf='mupdf'

#
# History
#
HISTFILE=~/.zsh_history
HISTSIZE=4096
SAVEHIST=4096
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
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS
setopt INTERACTIVECOMMENTS

#
# Set prompt
#
precmd() {
  local last=$?

  # Status
  if [ "$last" -eq 0 ]; then
    PROMPT='; '
    RPROMPT=''
  else
    PROMPT="%{$fg[red]%}; %{$reset_color%}"
    RPROMPT="%{$fg[red]%}# $last%{$reset_color%}"
  fi

  # Window title
  echo -ne "\e]1;$PWD - $(git rev-parse --abbrev-ref HEAD 2>/dev/null)\a"
}

col() {
  awk "{ print \$$1 }"
}

