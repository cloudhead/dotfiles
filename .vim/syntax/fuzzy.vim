" Vim syntax file
" Language:	    Fuzzy buffer
" Maintainer:	Alexis Sellier
" File Types:	.fuzzy
"
syntax match fuzzyTodo       'TODO'
syntax match fuzzyFixme      'FIXME'
syntax match fuzzyLocation   '^[0-9_a-zA-Z-/]\+\(\.[a-zA-Z]\+\)\?:[0-9]\+:'

hi fuzzyTodo          cterm=bold ctermfg=cyan
hi fuzzyFixme         cterm=bold ctermfg=magenta
hi link fuzzyLocation Comment
