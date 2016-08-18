#
# fzy shell integration
#
if [[ $- != *i* ]]; then
	return
fi

__fzy_fsel () {
	setopt localoptions pipefail 2> /dev/null
	command ag -g '' | fzy | while read -r item; do
		echo -n "${(q)item}"
	done
	local ret=$?
  echo
	return $ret
}

# Place the selected file path on the command line.
fzy-file-widget () {
	LBUFFER="${LBUFFER}$(__fzy_fsel)"
	local ret=$?
	zle redisplay
	typeset -f zle-line-init >/dev/null && zle zle-line-init
	return $ret
}
zle     -N   fzy-file-widget
bindkey '^P' fzy-file-widget

# Place the selected command from history on the command line.
fzy-history-widget () {
  local selected num
	setopt localoptions noglobsubst pipefail 2> /dev/null
  selected=( $(fc -l -r 1 | fzy -q "${LBUFFER//$/\\$}") )
	local ret=$?
	if [[ -n ${selected} ]]; then
		num=${selected[1]}
		if [[ -n ${num} ]]; then
			zle vi-fetch-history -n ${num}
		fi
	fi
	zle redisplay
	typeset -f zle-line-init >/dev/null && zle zle-line-init
	return $ret
}
zle     -N   fzy-history-widget
bindkey '^R' fzy-history-widget
