#
# config.fish
#
if status is-interactive
  fish_vi_key_bindings

  set -g fish_greeting
  set fish_cursor_default block
  set fish_cursor_insert underscore
  set fish_cursor_replace_one underscore
  set fish_cursor_visual block

  if type -q keychain
    keychain --eval -Q --quiet ~/.ssh/id_rsa ~/.radicle/keys/radicle ~/.ssh/alexis.radiant.computer | source
  end

  function fish_mode_prompt
  end

  bind -M insert \cr history-pager
  bind \cz 'fg 2>/dev/null; commandline -f repaint'
  bind -M insert \cz 'fg 2>/dev/null; commandline -f repaint'

  if test -n "$SSH_CLIENT"; or test -n "$SSH_TTY"
    set -gx TERM xterm
  end

  set -gx GPG_TTY (tty)
  set -gx NNN_USE_EDITOR 1
  set -gx GOPATH ~
  set -gx PAGER "less -s -M +Gg"
  set -gx EZA_COLORS "reset:fi=37:di=36:ln=36:or=31:ex=32:sp=31:pi=33:so=33:bd=33:cd=33:xx=90:da=90:uu=37:gu=37:ur=90:uw=90:ux=32:gr=90:gw=90:gx=32:tr=90:tw=90:tx=32:sn=37:sb=90:lc=90"

  alias l "eza --color=auto --icons --group-directories-first"
  alias ll "eza -lag --color=auto --icons --group-directories-first"
  alias g git
  alias n "sudo netctl"
  alias m "make -j8"
  alias b yazi
  alias f fd
  alias mv "/bin/mv -i"
  alias img "sxiv -a"
  alias df "df -h"
  alias sys systemctl
  alias s systemctl
  alias pdf mupdf
  alias webserver "python -m http.server"
  alias pacman "sudo pacman --color=auto"
  alias vim nvim
  alias clip wl-copy
  alias irc weechat
  alias cloc tokei
  alias shred "shred -uvz"
  alias diskusage ncdu
  alias calc kalk
  alias t tree-git-ignore
  alias sql3 "sqlite3 -box"

  function e
    $EDITOR $argv
  end

  function weather
    curl "https://v2.wttr.in/$argv[1]"
  end

  function tree-git-ignore
    set -l ignored (git ls-files -ci --others --directory --exclude-standard 2>/dev/null \
      | string match -rv '^#.*$|^[[:space:]]*$' \
      | string replace -r '^/' '' \
      | string replace -r '/$' '')

    set -l ignored_filter ".git"
    if test (count $ignored) -gt 0
      set ignored_filter ".git|"(string join "|" $ignored)
    end

    /usr/bin/tree --prune -I "$ignored_filter" $argv
  end

  function find-and-replace
    rg "$argv[1]" --files-with-matches | xargs sed -i "s@$argv[1]@$argv[2]@g"
  end

  function pdf-slice
    qpdf $argv[1] --pages . $argv[2] -- $argv[3]
  end

  function col
    awk "{ print \$$argv[1] }"
  end

  source ~/.config/fish/pureish.fish
end
