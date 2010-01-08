" Tabs and Spaces
set tabstop=2
set shiftwidth=2
set softtabstop=2
set backspace=indent,eol,start
set expandtab
set autoindent
set smartindent
set smarttab

" Misc
set number
set ruler
set showcmd
set showmatch
set wildmenu
set nowrap
set hidden
set modeline
set autoread       " auto-reload modified files (with no local changes)
set nocompatible   " don't try to be compatible with vi
set ignorecase     " ignore case in search
set smartcase      " override ignorecase if uppercase is used in search string
set report=0       " report all changes
set laststatus=2   " always show status-line
set cursorline     " highlight current line

" Keep swap files in one of these 
set directory=~/tmp,/var/tmp,/tmp,.

" Let's see some useful info in the status line
set statusline=%F\ %m%r%w%y\ %=(%L\ loc)\ [#\%03.3b\ 0x\%02.2B]\ \ %l,%v\ \ %P

" Pretend javascript function literals have beautiful syntax
autocmd BufRead,BufWritePost *.js silent %s/function/->/
autocmd BufWritePre          *.js silent %s/->/function/

" Per file-type indentation
autocmd BufEnter *.js set softtabstop=4|set shiftwidth=4
autocmd BufEnter *.c  set softtabstop=4|set shiftwidth=4

" Better search
set hlsearch
set incsearch

" File-type
filetype on
filetype plugin on
filetype indent on

" Show trailing white-space
let ruby_space_errors = 1
let c_space_errors = 1
let javascript_space_errors = 1

" Easy command mode switch
inoremap kj <Esc>

" Fix backspace key in xterm
inoremap  <BS>

" Enable mouse in insert and normal mode
set mouse=in

" Create an empty line underneath without moving the cursor
noremap <CR> mlo<Esc>`l

" Indent with spacebar
noremap <space> >>

" Move easily between ^ and $
noremap <C-h> ^
noremap <C-l> $

" Syntax coloring
set t_Co=256
colorscheme cotingale
syntax enable

" Minimum window height = 0
set wmh=0

"
" Tabline
"
if exists("+showtabline")
  function! MyTabLine()
    let s = ''
    let t = tabpagenr()
    let i = 1

    while i <= tabpagenr('$')
      let buflist = tabpagebuflist(i)
      let winnr = tabpagewinnr(i)
      let s .= '%' . i . 'T'
      let s .= (i == t ? '%1*' : '%2*')
      let s .= (i == t ? '%#TabLineSel#' : '%#TabLine#')
      let file = bufname(buflist[winnr - 1])
      let file = fnamemodify(file, ':p:t')
      let file = (file == '') ? '[No Name]' : file
      let s .= ' ' . file . ' '
      let s .= winnr
      let s .= (getbufvar(buflist[winnr - 1], '&modified') ? '+ ' : ' ')
      let i = i + 1
    endwhile
    let s .= '%T%#TabLineFill#%='
    let s .= (tabpagenr('$') > 1 ? '%999XX' : 'X')
    return s
  endfunction
  set stal=2
  set tabline=%!MyTabLine()
endif
      
