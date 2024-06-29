##
# cloudhead - .zshrc
#

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  export TERM=xterm
fi

export GPG_TTY=$(tty)

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

if [ -f ~/.LESS_TERMCAP ]; then
  export LESS="--RAW-CONTROL-CHARS"
  source ~/.LESS_TERMCAP
fi

# K8 integration. We lazy load because it's slow otherwise.
function kubectl() {
	if ! type __start_kubectl >/dev/null 2>&1; then
		unfunction "$0"
		source <(kubectl completion zsh)
	fi
	command kubectl "$@"
}

# Fzy history search doesn't sort things in a useful way, so we use zsh for now.
bindkey '^R' history-incremental-search-backward

#
# ls
#
alias l="ls -lFGhL --color=auto --group-directories-first"
alias ll='ls -lFAGh --color=auto --group-directories-first'

#
# Aliases
#
alias g='git'
alias n='sudo netctl'
alias mk='make'
alias mv='/bin/mv -i'
alias ..='cd ..'
alias img='sxiv -a'
alias df='df -h'
alias sys='systemctl'
alias s='systemctl'
alias x='startx'
alias web='chromium &'
alias e=$EDITOR
alias pdf='mupdf'
alias webserver='python -m http.server'
alias pacman='sudo pacman --color=auto'
alias vim=nvim
alias clip='xclip -sel clip'
alias irc='weechat'
alias cloc='tokei'
alias shred='shred -uvz'
alias diskusage='ncdu'
alias calc=kalk
alias t=tree-git-ignore
alias sql3='sqlite3 -box'

function weather {
  curl "https://v2.wttr.in/$1"
}

function tree-git-ignore {
  local ignored=$(git ls-files -ci --others --directory --exclude-standard)
  local ignored_filter=$(echo "$ignored" \
    | egrep -v "^#.*$|^[[:space:]]*$" \
    | sed 's~^/~~' \
    | sed 's~/$~~' \
    | tr "\\n" "|")

  /usr/bin/tree --prune -I ".git|${ignored_filter: : -1}" "$@"
}

function find-and-replace {
  rg "$1" --files-with-matches | xargs sed -i "s@$1@$2@g"
}

function pdf-slice {
  qpdf $1 --pages . $2 -- $3
}

export NNN_USE_EDITOR=1

#
# History
#
HISTFILE=~/.zsh_history
HISTSIZE=65536
SAVEHIST=65536
REPORTTIME=10

# Treat the '!' character specially during expansion.
setopt BANG_HIST
# Write the history file in the ":start:elapsed;command" format.
setopt EXTENDED_HISTORY
# Write to the history file immediately, not when the shell exits.
setopt INC_APPEND_HISTORY
# Expire duplicate entries first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST
# Don't record an entry that was just recorded again.
setopt HIST_IGNORE_DUPS
# Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_ALL_DUPS
# Do not display a line previously found.
setopt HIST_FIND_NO_DUPS
# Don't write duplicate entries in the history file.
setopt HIST_SAVE_NO_DUPS
# Remove superfluous blanks before recording entry.
setopt HIST_REDUCE_BLANKS

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
  local indicator=">"
  local extra=""

  if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    indicator="$(whoami)@$(cat /etc/hostname) #"
  fi

  if [ -n "$RAD_HOME" ]; then
    indicator="🌱"
    extra=" RAD_HOME=$RAD_HOME"
  fi

  # Status
  if [ "$last" -eq 0 ]; then
    PROMPT="%{$fg[blue]%}$indicator %{$reset_color%}"
    RPROMPT="$extra"
  else
    PROMPT="%{$fg[red]%}$indicator %{$reset_color%}"
    RPROMPT=" $last"
  fi

  if [ "$RPROMPT" != "" ]; then
    RPROMPT="%{$fg[red]%}#$RPROMPT%{$reset_color%}"
  fi

  # Set the window title to the pwd.
  print -Pn "\e]2;%~\a"
}

preexec() {
  # Set the window title to the command and pwd.
  print -Pn "\e]2;$1 [%~]\a"
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
