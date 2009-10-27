" Tabs and Spaces
set tabstop=2
set shiftwidth=2
set softtabstop=2
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
set nocompatible

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

" Go to next tab
nmap <Tab> :tabn<CR>
nmap <S-Tab> :tabp<CR>

" Syntax coloring
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
