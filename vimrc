" Tabs and Spaces
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent

" Misc
set number
set ruler
set showcmd
set showmatch
set wildmenu
set nowrap

" Easy command mode switch
inoremap jj <Esc>

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
