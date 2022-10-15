" Vim syntax file
" Language:	    Todo file
" Maintainer:	Alexis Sellier
" File Types:	.todo
"
syntax match todoOther       '^.*'
syntax match todoDone        '^\s*\[[Xx]\].*'
syntax match todoNormal      '^\s*\[ \]\s*.*'
syntax match todoComment     '^\s*%.*'
syntax match todoComment     '^\s*;.*'
syntax match todoCheckbox    '^\s*\[\s\]'
syntax match todoUrgent      '^\s*\[!\]\s.*$'
syntax match todoCanceled    '^\s*\[/\]\s.*$'
syntax match todoImportant   '\s!!'
syntax match todoImportant   '\s![a-zA-Z0-9-]\+'
syntax match todoAssignee    '@[a-z]\+\>'
syntax match todoCode        '`[^`]\+`'
