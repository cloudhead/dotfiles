#
# config.fish
#
set -gx PATH \
  "$HOME/bin" \
  "$HOME/.local/bin" \
  "$HOME/.yarn/bin" \
  "$HOME/.radicle/bin" \
  "$HOME/.cargo/bin" \
  "$HOME/.npm/bin" \
  "$HOME/.local/share/gem/ruby/3.4.0/bin" \
  "$HOME/.proto/shims" \
  "$HOME/.proto/bin" \
  $PATH
set -gx EDITOR nvim
set -gx VISUAL nvim
set -gx RIPGREP_CONFIG_PATH "$HOME/.rgrc"
set -gx CARGO_BUILD_JOBS 12

if status is-interactive
  fish_vi_key_bindings

  set -g fish_greeting
  set -g fish_autosuggestion_enabled 0

  set fish_cursor_default block
  set fish_cursor_insert underscore
  set fish_cursor_replace_one underscore
  set fish_cursor_visual block

  if set -q SSH_AUTH_SOCK
    if not test -S "$SSH_AUTH_SOCK"
      set -e SSH_AUTH_SOCK
      set -e SSH_AGENT_PID
    else if type -q ssh-add
      ssh-add -l >/dev/null 2>&1
      if test $status -eq 2
        set -e SSH_AUTH_SOCK
        set -e SSH_AGENT_PID
      end
    end
  end

  if status is-login; and type -q keychain
    keychain --eval --quiet --noinherit \
      ~/.ssh/id_ed25519 \
      ~/.ssh/id_rsa \
      ~/.ssh/id_rsa_share \
      ~/.radicle/keys/radicle \
      ~/.ssh/alexis.radiant.computer | source
  else if not set -q SSH_AUTH_SOCK
    set -l keychain_env ~/.keychain/(uname -n)-fish
    if test -r "$keychain_env"
      source "$keychain_env"
    end
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

  alias l "eza --width=50 --color=auto --icons --group-directories-first"
  alias ll "eza -lag --color=auto --icons --group-directories-first"
  alias g git
  alias n "sudo netctl"
  alias m "make -j8"
  alias b yazi
  alias f fd
  alias mv "/bin/mv -i"
  alias img swayimg
  alias df "df -h"
  alias sys systemctl
  alias s systemctl
  alias pdf zathura
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

  if type -q fzf
    fzf --fish | source
  end
end
