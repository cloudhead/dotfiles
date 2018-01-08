##
# cloudhead - .zshrc
#

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

# Fzy integration
if command -v fzy >/dev/null 2>&1 && test -f ~/.fzy.zsh; then
  source ~/.fzy.zsh
fi

if command -v kubectl >/dev/null 2>&1; then
	source <(kubectl completion zsh)
fi

# Fzy history search doesn't sort things in a useful way, so we use zsh for now.
bindkey '^R' history-incremental-search-backward

#
# ls
#
LS_IGNORE="Dropbox" # ~/Dropbox is symlinked.

alias ls="/bin/ls -I $LS_IGNORE"
alias l="/bin/ls -lFGhL --color=auto -I $LS_IGNORE"
alias ll='/bin/ls -lFAGh --color=auto'

#
# Aliases
#
alias g='git'
alias n='sudo netctl'
alias mv='/bin/mv -i'
alias ..='cd ..'
alias img='sxiv -a'
alias df='df -h'
alias sys='systemctl'
alias x='startx'
alias web='chromium &'
alias e=$EDITOR
alias pdf='mupdf'
alias webserver='python2 -m SimpleHTTPServer'
alias pacman='sudo pacman --color=auto'
alias netctl='sudo netctl'
alias vim=nvim
alias clip='xclip -sel clip'

#
# History
#
HISTFILE=~/.zsh_history
HISTSIZE=65536
SAVEHIST=65536
REPORTTIME=10

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.

autoload up-line-or-beginning-search
autoload down-line-or-beginning-search

zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "\e[A" up-line-or-beginning-search
bindkey "\e[B" down-line-or-beginning-search

#
# Options
#
setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS
setopt LOCAL_TRAPS
setopt EXTENDED_HISTORY
setopt PROMPT_SUBST
setopt CORRECT
setopt COMPLETE_IN_WORD
setopt NO_IGNORE_EOF
setopt AUTO_CD
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

#
# Vi-mode
#
bindkey -v
export KEYTIMEOUT=1
#
zle-keymap-select zle-line-init() {
  # Check ~/.st/config.h for the cursor escape sequences.
  case $KEYMAP in
    vicmd)      print -n -- "\e[2 q";;
    viins|main) print -n -- "\e[4 q";;
  esac

  zle reset-prompt
  zle -R
}

zle-line-finish() {
  print -n -- "\e[2 q"
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

col() {
  awk "{ print \$$1 }"
}

#
# Switch to `fg` process with Ctrl-Z
#
fg-command() {
  if [[ ! $#BUFFER -eq 0 ]]; then
    zle push-input
  fi
  BUFFER="fg"
  zle accept-line
}
zle -N fg-command
bindkey '^Z' fg-command

# Zsh syntax highlighting
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
