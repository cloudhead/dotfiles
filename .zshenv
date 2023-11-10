##
# cloudhead - .zshenv
#
[ -f ~/.profile ] && source ~/.profile
[ -f ~/.env ]     && source ~/.env
[ -f ~/.awsrc ]   && source ~/.awsrc

export GOPATH=~
export PAGER="less -s -M +Gg"

if command -v nvim >/dev/null 2>&1; then
  export VISUAL=nvim
  export EDITOR=nvim
else
  export VISUAL=vim
  export EDITOR=vim
fi

. "$HOME/.cargo/env"
