# Pure-inspired prompt, kept local to avoid a plugin dependency.
# Based on Pure fish's core ideas: pwd, git state, duration, SSH context,
# failure status, and a prompt symbol that turns red on error.

function __pureish_git_prompt
  command git rev-parse --is-inside-work-tree >/dev/null 2>&1; or return

  set -l branch (command git symbolic-ref --quiet --short HEAD 2>/dev/null)
  if test -z "$branch"
    set branch (command git rev-parse --short HEAD 2>/dev/null)
  end
  test -n "$branch"; or return
  printf '%s' $branch
end

function __pureish_duration
  test "$CMD_DURATION" -ge 5000 2>/dev/null; or return

  if test "$CMD_DURATION" -lt 60000
    printf '%ss' (math --scale=1 "$CMD_DURATION / 1000")
  else
    set -l minutes (math --scale=0 "$CMD_DURATION / 60000")
    set -l seconds (math --scale=0 "($CMD_DURATION % 60000) / 1000")
    printf '%sm%ss' $minutes $seconds
  end
end

function fish_prompt
  set -g __pureish_last_status $status
  set -g __pureish_extra
  set -l symbol '>'
  set -l ssh
  if test -n "$SSH_CLIENT"; or test -n "$SSH_TTY"
    set ssh (whoami)'@'(hostname -s)
  end

  set -l git (__pureish_git_prompt)

  if test -n "$ssh"
    set_color brblack
    printf '%s ' $ssh
  end

  set_color 555555
  printf '%s' (prompt_pwd)

  if test -n "$git"
    set_color brblack
    printf ' %s' $git
  end

  printf '\n'

  if test $__pureish_last_status -eq 0
    set_color blue
  else
    set_color red
  end

  printf '%s ' $symbol
  set_color normal
end

function fish_right_prompt
  set -l parts
  set -l duration (__pureish_duration)

  if test -n "$duration"
    set parts $parts $duration
  end

  if test -n "$__pureish_extra"
    set parts $parts $__pureish_extra
  else if test "$__pureish_last_status" -ne 0
    set parts $parts "#$__pureish_last_status"
  end

  test (count $parts) -gt 0; or return
  set_color brblack
  printf '%s' (string join ' ' $parts)
  set_color normal
end

function fish_title
  set -l pwd (string replace -r "^$HOME(?=/|\$)" "~" -- "$PWD")
  set -l process fish
  if test (count $argv) -gt 0; and test -n "$argv[1]"
    set process (string split -m1 ' ' -- "$argv[1]")[1]
  end
  printf '%s %s' "$pwd" "$process"
end
