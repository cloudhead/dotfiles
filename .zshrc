##
# cloudhead - .zshrc
#
export FZF_DEFAULT_COMMAND='ag --hidden -U -g ""'
export FZF_DEFAULT_OPTS='--prompt="/ " --color=16 --exact'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

[ -f ~/.profile ] && source ~/.profile
[ -f ~/.awsrc ]   && source ~/.awsrc

# `ls` colors
if [ -f ~/.dircolors ]; then
  eval $(dircolors -b ~/.dircolors)
fi

#
# Includes
#
autoload colors && colors
autoload -U compinit && compinit
autoload -U complist

# Set input mode to vi
set -o vi

# Bind <C-r> to history search
if [ command -v fzf >/dev/null 2>&1 ]; then
  bindkey '^R' fzf-history-widget
  bindkey '^P' fzf-file-widget
else
  bindkey '^R' history-incremental-search-backward
fi

#
# Aliases
#
alias g='git'
alias ls='ls -p'
alias l='ls -lFGh --color=auto'
alias ll='ls -lFAGh --color=auto'
alias mv='/bin/mv -i'
alias ..='cd ..'
alias img='sxiv'
alias df='df -h'
alias sys='systemctl'
alias x='startx'
alias web='chromium &'
alias e=$EDITOR
alias pdf='mupdf'
alias webserver='python2 -m SimpleHTTPServer'
alias pacman='sudo pacman --color=auto'

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
  local remote=""

  if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    remote=" $(whoami)@$(hostname)"
  fi

  # Status
  if [ "$last" -eq 0 ]; then
    PROMPT='; '
    RPROMPT="$remote"
  else
    PROMPT="%{$fg[red]%}; %{$reset_color%}"
    RPROMPT=" $last"
  fi

  if [ "$RPROMPT" != "" ]; then
    RPROMPT="%{$fg[red]%}#$RPROMPT%{$reset_color%}"
  fi
}

col() {
  awk "{ print \$$1 }"
}

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
