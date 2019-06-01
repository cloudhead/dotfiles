" Vim syntax file
" Language:	    Todo file
" Maintainer:	Alexis Sellier
" File Types:	.todo
"
syntax match todoOther    '^.*'
syntax match todoDone     '^\s*\[[Xx]\].*'
syntax match todoNormal   '^\s*\[ \]\s*.*'
syntax match todoComment  '^\s*;.*'
syntax match todoCheckbox '^\s*\[\s\]'

highlight link todoOther    Keyword
highlight link todoCheckbox Comment
highlight link todoComment  Comment
highlight link todoDone     Comment
highlight link todoNormal   Normal
