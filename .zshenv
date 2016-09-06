##
# cloudhead - .zshenv
#

[ -f ~/.profile ] && source ~/.profile
[ -f ~/.env ]     && source ~/.env

export VISUAL=vim
export GOPATH=~

if command -v nvim >/dev/null 2>&1; then
  export EDITOR=nvim
else
  export EDITOR=vim
fi

