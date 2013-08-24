"
" .vimrc
"
" Tabs and Spaces
set tabstop=4
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
set showmatch
set wildmenu
set wildmode=list,full
set nowrap
set hidden
set modeline
set hlsearch
set incsearch
set autoread                        " auto-reload modified files (with no local changes)
set nocompatible                    " don't try to be compatible with vi
set ignorecase                      " ignore case in search
set smartcase                       " override ignorecase if uppercase is used in search string
set report=0                        " report all changes
set laststatus=2                    " always show status-line
set nocursorline                    " highlight current line
set scrolloff=4
set nofoldenable
set timeoutlen=200                  " set timeout between key sequences
set encoding=utf-8
set background=dark
set mouse=vin                       " Enable mouse in insert and normal mode
set directory=~/tmp,/var/tmp,/tmp,. " Keep swap files in one of these 
set wmh=0                           " Minimum window height = 0
set showcmd


" Per file-type indentation
autocmd BufEnter *.hs  set softtabstop=4|set shiftwidth=4
autocmd BufEnter *.js  set softtabstop=4|set shiftwidth=4
autocmd BufEnter *.go  set tabstop=4|set shiftwidth=4|set noexpandtab
autocmd BufEnter *.c   set shiftwidth=4|set noexpandtab
autocmd BufEnter *.h   set shiftwidth=4|set noexpandtab
autocmd BufEnter *.lua set shiftwidth=2|set expandtab
autocmd BufEnter *.erl set softtabstop=4|set shiftwidth=4

" File-type
filetype on
filetype plugin on
filetype indent on

" Easy command mode switch
inoremap kj <Esc>
inoremap <C-l> <C-x><C-l>

noremap <C-j> }
noremap <C-k> {

" Move easily between ^ and $
noremap <C-h> ^
noremap <C-l> $
noremap j gj
noremap k gk

" \m to make
map <Leader>m :make<Return>

" Syntax coloring
set t_Co=256
syntax enable

function! CleverTab()
  if strpart(getline('.'), 0, col('.')-1) =~ '^\s*$'
    return "\<Tab>"
  else
    return "\<C-N>"
  endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>
