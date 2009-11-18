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

" Directory where swap files are kept (in order of preferense)
set directory=~/tmp,/var/tmp,/tmp,.

" Set Status-line with useful info
set statusline=%F\ %m%r%w(%Y)\ %=(%L\ loc)\ [#\%03.3b\ 0x\%02.2B]\ \ %l,%v\ \ %P

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

" Easy command mode switch
inoremap jj <Esc>

" Move cursor inside delimiters
inoremap [] []<Left>
inoremap "" ""<Left>
inoremap '' ''<Left>
inoremap () ()<Left>
inoremap {} {}<Left>
inoremap `` ``<Left>
inoremap <> <><Left>

" Move with h & l in input mode
inoremap <C-h> <Left>
inoremap <C-l> <Right>

" Fix backspace key in xterm
inoremap  <BS>

" Enable mouse in insert and normal mode
set mouse=in

" Create an empty line underneath without moving the cursor
nmap <CR> mlo<Esc>`l

" Indent with spacebar
nmap <space> >>

" Syntax coloring
set t_Co=256
colorscheme cloudhead
syntax enable

" Minimum window height = 0
set wmh=0

" Move lines of text around
nnoremap <C-S-j> mz:m+<CR>`z==
nnoremap <C-S-k> mz:m-2<CR>`z==
inoremap <C-S-j> <Esc>:m+<CR>==gi
inoremap <C-S-k> <Esc>:m-2<CR>==gi
vnoremap <C-S-j> :m'>+<CR>gv=`<my`>mzgv`yo`z
vnoremap <C-S-k> :m'<-2<CR>gv=`>my`<mzgv`yo`z

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
      
