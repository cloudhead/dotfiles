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
set nocompatible   " don't try to be compatible with vi
set ignorecase     " ignore case in search
set smartcase      " override ignorecase if uppercase is used in search string

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

" Go to next tab
nmap <Tab> gt
nmap <S-Tab> gT

" auto `cd` to directory, when opening a file
autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /

" Syntax coloring
set t_Co=256
colorscheme cloudhead
syntax enable

" Easy view switching
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
set wmh=0

" Move lines of text around
nnoremap <C-S-j> mz:m+<CR>`z==
nnoremap <C-S-k> mz:m-2<CR>`z==
inoremap <C-S-j> <Esc>:m+<CR>==gi
inoremap <C-S-k> <Esc>:m-2<CR>==gi
vnoremap <C-S-j> :m'>+<CR>gv=`<my`>mzgv`yo`z
vnoremap <C-S-k> :m'<-2<CR>gv=`>my`<mzgv`yo`z
