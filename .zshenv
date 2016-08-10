##
# cloudhead - .zshenv
#

export PATH=~/bin:~/.cabal/bin:$PATH
export VISUAL=vim
export GOPATH=~

if command -v nvim >/dev/null 2>&1; then
  export EDITOR=nvim
else
  export EDITOR=vim
fi

