syn match   solKeyword /\(msg\.\)\@<=\(sender\|value\)/
syn keyword solKeyword override immutable calldata
syn match   solNumber  /\<\d[_0-9]\+\([eE]\d\+\)\?\>/
