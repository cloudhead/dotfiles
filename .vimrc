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
set timeoutlen=500                  " Set timeout between key sequences
set background=dark
set mouse=vin                       " Enable mouse in insert and normal mode
set directory=~/tmp,/var/tmp,/tmp,. " Keep swap files in one of these
set wmh=0                           " Minimum window height = 0
set showcmd
set updatetime=250                  " How long before 'CursorHold' event
set nobackup
set nowritebackup
set noswapfile
set nostartofline
set cmdheight=1
set matchtime=2                     " Shorter brace match time
set virtualedit=block
set tags+=.tags
set undofile
set gdefault                        " Always use /g with %s/
set colorcolumn=80
set list
set listchars=tab:·\ ,eol:¬,trail:█
set fillchars=diff:\ ,vert:│
set diffopt=filler,vertical,foldcolumn:0
set lazyredraw                      " Stop vim from freaking out all the time
set statusline=%<%f\ %h%m%r%=%{Hi()}\ %y\ \ %-14(%{&sw}:%{&sts}:%{&ts}%)%-14.(%l,%c%V%)\ %P

" We don't use tabs much, but at least try and show less cruft
function! Tabline()
  let s = ''
  for i in range(tabpagenr('$'))
    let tab = i + 1
    let winnr = tabpagewinnr(tab)
    let buflist = tabpagebuflist(tab)
    let bufnr = buflist[winnr - 1]
    let bufname = bufname(bufnr)

    let s .= (tab == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' ' . (!empty(bufname) ? fnamemodify(bufname, ':t') : '[No Name]') . ' '
  endfor
  return s
endfunction
set tabline=%!Tabline()

if !has("nvim")
  set nocompatible                  " Don't try to be compatible with vi
  set ttyfast
  set t_Co=256
endif

let mapleader = "\<Space>"
let g:gitgutter_sign_column_always = 1
let g:gitgutter_grep_command = 'ag -g'
let g:gitgutter_map_keys = 0
let g:gitgutter_eager = 1
let g:gitgutter_realtime = 0
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDCustomDelimiters = { 'haskell': { 'left': '-- ', 'right': '' } }
let g:buftabline_show = 1

let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" Deoplete (autocomplete)
if has("nvim")
  let g:deoplete#enable_at_startup = 0
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

" Update gutter whenever possible
au FocusLost * :GitGutter

" Save on focus lost
au FocusLost * call s:SaveOnFocusLost()
function! s:SaveOnFocusLost()
  if !empty(expand('%:p')) && &modified
    write
  endif
endfunction

" Per file-type indentation
au FileType haskell     setlocal sts=4 sw=4 expandtab
au FileType javascript  setlocal sts=4 sw=4 expandtab
au FileType css         setlocal ts=4  sw=4 noexpandtab
au FileType go          setlocal ts=4  sw=4 noexpandtab
au FileType c,cpp       setlocal       sw=4 noexpandtab
au FileType lua         setlocal       sw=2 expandtab
au FileType sh,zsh      setlocal ts=2  sw=2 noexpandtab
au FileType vim,ruby    setlocal sts=2 sw=2 expandtab
au FileType help        setlocal ts=4  sw=4 noexpandtab

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
au FileType haskell setlocal omnifunc=necoghc#omnifunc
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

if executable('haskell-tags')
  au BufWritePost *.hs  silent !haskell-tags % '.tags'
  au BufWritePost *.hsc silent !haskell-tags % '.tags'
endif

" File-type
filetype on
filetype plugin on
filetype indent on

nnoremap <Space> <NOP>

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
nnoremap <C-n>           *N
nnoremap c*              *Ncgn
nnoremap <Leader>h       :nohl<CR>

" Git
nnoremap <Leader>gc     :Gcommit -v<CR>
nnoremap <Leader>gc.    :Gwrite <Bar> Gcommit -v<CR>
nnoremap <Leader>gca    :Git commit -a<CR>
nnoremap <Leader>ga     :GitGutterStageHunk<CR>
nnoremap <Leader>ga.    :Gwrite<CR>
nnoremap <Leader>gu     :GitGutterUndoHunk<CR>
nnoremap <Leader>gp     :GitGutterPreviewHunk<CR>
nnoremap <Leader>n      :cnext<CR>
nnoremap <Leader>p      :cprev<CR>

 " Select recently pasted text
nnoremap <leader>p       V`]

" Switch buffers easily
nnoremap <Tab>   <C-^>

" Actually easier to type and I do it by mistake anyway
cnoreabbrev W w
cnoreabbrev Q q

" File navigation/search
nnoremap <C-p>      :FuzzyOpen<CR>

" Navigate relative to the current file
cmap     %/         %:p:h/

map <Leader>m       :make<CR>
map <Leader>.       @:
map <Leader>e       :e ~/.vimrc<CR>
map <Leader>s       :source ~/.vimrc<CR>

" Commenting
nmap <C-_>           <Plug>CommentaryLine
xmap <C-_>           <Plug>Commentary

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

" Get highlight group under cursor
command! Hi call s:ToggleHi()
function! s:ToggleHi()
  if exists('g:show_hi')
    unlet g:show_hi
  else
    let g:show_hi = 1
  endif
endfunction
function! Hi()
  if exists('g:show_hi')
    return synIDattr(synID(line("."), col("."), 1), "name")
  endif
  return ''
endfunction

call plug#begin()

Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'mileszs/ack.vim'
Plug 'eagletmt/neco-ghc'
Plug 'shougo/deoplete.nvim'
Plug 'neovimhaskell/haskell-vim'
Plug 'bronson/vim-visual-star-search'
Plug 'cloudhead/neovim-ghcid'
Plug 'cloudhead/neovim-fuzzy'

call plug#end()
