##
# odyslam - .zshrc
#
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  export TERM=xterm
fi

export GPG_TTY=$(tty)


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

# Fzy history search doesn't sort things in a useful way, so we use zsh for now.
bindkey '^R' history-incremental-search-backward

#
# Aliases
#
# Foundry
alias fb="forge build"
alias ft="forge test"
# GitHub
alias g='git'
alias ga="git -a ."
alias gc"git commit"
# Cargo
alias c="cargo"
alias cc="cargo check"
alias cb="cargo build"
alias ct="cargo test"
alias cl="cargo check --all && cargo test --all --all-features && cargo +nightly fmt -- --check && cargo +nightly clippy --all --all-features -- -D warnings"
# Wrangler -- Cloudflare Workers
alias wd="wrangler dev"
alias wp="wrangler publish"
# Javascript
alias y="yarn"
# Bash
alias v="nvim"
alias mk='make'
alias mv='/bin/mv -i'
alias ..='cd ..'
alias df='df -h'
alias e=$EDITOR
alias vim=nvim
alias vi=nvim
alias diskusage='ncdu'
alias t=tree-git-ignore
alias tmh="tmux splitw -h"
alias tmv="tmux splitw -v"
alias ga='gatsby'
alias gad='gatsby develop'
alias gac='gatsby clean'
alias cat='bat'
function tree-git-ignore {
  local ignored=$(git ls-files -ci --others --directory --exclude-standard)
  local ignored_filter=$(echo "$ignored" \
    | egrep -v "^#.*$|^[[:space:]]*$" \
    | sed 's~^/~~' \
    | sed 's~/$~~' \
    | tr "\\n" "|")

  /usr/bin/tree --prune -I ".git|${ignored_filter: : -1}" "$@"
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


  if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    indicator="$(whoami)@$(cat /etc/hostname) #"
  fi
  # Set the window title to the pwd.
  print -Pn "\e]2;%~\a"
}

preexec() {
  # Set the window title to the command and pwd.
  print -Pn "\e]2;$1 [%~]\a"
}

export PATH=~/bin:~/.local/bin:~/.yarn/bin:~/.gcloud/bin:~/.cabal/bin:~/.cargo/bin:~/.gem/ruby/2.6.0/bin:~/.radicle/bin:~/.npm-packages/bin:$PATH
export EDITOR=nvim
export VISUAL=nvim
export RIPGREP_CONFIG_PATH=$HOME/.rgrc
export ZSH="/Users/odys/.oh-my-zsh"

eval "$(rbenv init -)"
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes


if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh;
fi # added by Nix installer


# Oh my zsh

ZSH_THEME="agnoster"
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

ZSH_COMMAND_TIME_MIN_SECONDS=1

plugins=(git docker copybuffer textmate tmux sudo zsh-syntax-highlighting zsh-autosuggestions command-time zsh-vi-mode)
ZSH_TMUX_AUTOSTART="true"
export SSH_AUTH_SOCK=/Users/odys/Library/Containers/com.maxgoedjen.Secretive.SecretAgent/Data/socket.ssh

source $ZSH/oh-my-zsh.sh
source ~/.zsh/zsh_secrets
if [ -e /Users/odys/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/odys/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
export PATH="/usr/local/sbin:$PATH"

export PATH="$PATH:/Users/odys/.foundry/bin"
