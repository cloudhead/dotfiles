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
set cindent
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
set autoread                        " Auto-reload modified files (with no local changes)
set ignorecase                      " Ignore case in search
set smartcase                       " Override ignorecase if uppercase is used in search string
set report=0                        " Report all changes
set laststatus=2                    " Always show status-line
set nocursorline                    " Highlight current line
set scrolloff=4
set nofoldenable
set timeoutlen=200                  " Set timeout between key sequences
set background=dark
set mouse=vin                       " Enable mouse in insert and normal mode
set directory=~/tmp,/var/tmp,/tmp,. " Keep swap files in one of these
set wmh=0                           " Minimum window height = 0
set showcmd
set updatetime=250                  " How long before 'CursorHold' event
set nobackup
set nowritebackup
set noswapfile
set nolazyredraw
set nostartofline
set cmdheight=1
set matchtime=2                     " Shorter brace match time
set virtualedit=block
set tags+=.tags
set undofile
set gdefault                        " Always use /g with %s/
set colorcolumn=80
set list
set listchars=tab:·\ ,eol:¬,trail:·

if !has("nvim")
  set nocompatible                  " Don't try to be compatible with vi
  set ttyfast
  set t_Co=256
endif

let mapleader = ","
let g:gitgutter_sign_column_always = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDCustomDelimiters = { 'haskell': { 'left': '-- ', 'right': '' } }

let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" Deoplete (autocomplete)
if has("nvim")
  let g:deoplete#enable_at_startup = 1
  let g:deoplete#disable_auto_complete = 1
  inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ deoplete#mappings#manual_complete()
  function! s:check_back_space()
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction
endif

" Save on focus lost
au FocusLost * call s:SaveOnFocusLost()
function! s:SaveOnFocusLost()
  if !empty(expand('%:p'))
    write
  endif
endfunction

" Per file-type indentation
au FileType haskell     set sts=4 sw=4 expandtab
au FileType javascript  set sts=4 sw=4 expandtab
au FileType go          set ts=4  sw=4 noexpandtab
au FileType c,cpp       set       sw=4 noexpandtab
au FileType lua         set       sw=2 expandtab
au FileType sh,zsh      set ts=2  sw=2 noexpandtab
au FileType vim         set sts=2 sw=2 expandtab

if executable('haskell-tags')
  au BufWritePost *.hs  silent !haskell-tags % '.tags'
  au BufWritePost *.hsc silent !haskell-tags % '.tags'
endif

" Remove trailing whitespace on save
autocmd BufWritePre * call s:StripTrailing()
function! s:StripTrailing()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfunction

" Haskell
let g:haskellmode_completion_ghc = 0
let g:haskell_enable_quantification = 1
let g:haskell_indent_where = 2
let g:haskell_indent_case = 4
let g:haskell_indent_guard = 4
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
au FileType haskell setlocal makeprg=stack\ build
au FileType haskell setlocal errorformat=
                \%-G,
                \%-Z\ %#,
                \%W%f:%l:%c:\ Warning:\ %m,
                \%E%f:%l:%c:\ %m,
                \%E%>%f:%l:%c:,
                \%+C\ \ %#%m,
                \%W%>%f:%l:%c:,
                \%+C\ \ %#%tarning:\ %m,

" File-type
filetype on
filetype plugin on
filetype indent on

" Easy command mode switch
inoremap kj <Esc>
inoremap <C-l> <C-x><C-l>

" Jump to high/low and scroll
noremap <C-k> H{
noremap <C-j> L}

" Move easily between ^ and $
noremap <C-h> ^
noremap <C-l> $
noremap j gj
noremap k gk

" Like '*' but stays on the original word
nnoremap <Tab>           *N
nnoremap c*              *Ncgn
nnoremap <Leader><Tab>   :nohl<CR>

 " Select recently pasted text
nnoremap <leader>p       V`]

" Remap arrow keys to something useful
nnoremap <Left>  :cprev<CR>
nnoremap <Right> :cnext<CR>
nnoremap <Up>    :bprev<CR>
nnoremap <Down>  :bnext<CR>

" File navigation/search
nnoremap <C-f>      :Ack!<Space>
nnoremap <C-p>      :FuzzyOpen<CR>
nnoremap <BS>       :b#

" Navigate relative to the current file
cmap     %/         %:p:h/

" Use standard regexp when searching
nnoremap /          /\v
vnoremap /          /\v

map <Leader>m       :make<CR>
map <C-_>           <Plug>NERDCommenterToggle
map <Leader>.       @:
map <Leader>e       :e ~/.vimrc<CR>
map <Leader>s       :source ~/.vimrc<CR>

if has("nvim")
  tnoremap <Esc> <C-\><C-n>
endif

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" Syntax coloring
syntax enable

try
  colorscheme shady
catch
endtry

" Profiling
command! ProfileStart call s:ProfileStart()
function! s:ProfileStart()
  profile start profile
  profile func *
  profile file *
endfunction

command! ProfileStop call s:ProfileStop()
function! s:ProfileStop()
  profile stop
  tabnew profile
endfunction

call plug#begin()

Plug 'scrooloose/nerdcommenter'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'mileszs/ack.vim'
Plug 'eagletmt/neco-ghc'
Plug 'shougo/deoplete.nvim'
Plug 'neovimhaskell/haskell-vim'
Plug 'cloudhead/neovim-ghcid'
Plug 'cloudhead/neovim-fuzzy'

call plug#end()
