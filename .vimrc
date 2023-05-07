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
set wildmode=full
set nowrap
set hidden
set modeline
set hlsearch
set incsearch
set autoread                        " Auto-reload modified files (with no local changes)
set ignorecase                      " Ignore case in search
set smartcase                       " Override ignorecase if uppercase is used in search string
set report=0                        " Report all changes
set laststatus=3                    " Always show status-line
set nocursorline                    " Highlight current line
set scrolloff=4
set nofoldenable
set timeoutlen=200                  " Set timeout between key sequences
set background=dark
set mouse=a                         " Enable mouse in all modes
set directory=~/tmp,/var/tmp,/tmp,. " Keep swap files in one of these
set wmh=0                           " Minimum window height = 0
set showcmd
set updatetime=250                  " How long before 'CursorHold' event
set nobackup
set nowritebackup
set noswapfile
set nostartofline
set noshowmode                      " Don't show stuff like `-- INSERT --`
set foldlevel=99                    " Open all folds by default
set cmdheight=1
set matchtime=2                     " Shorter brace match time
set virtualedit=block
set tags+=.tags
set tags+=codex.tags
set undofile
set gdefault                        " Always use /g with %s/
set colorcolumn=80
set list
set listchars=tab:·\ ,eol:¬,trail:█
set fillchars=diff:\ ,vert:│
set diffopt=filler,vertical,foldcolumn:0
set statusline=%<%f\ (%{gitbranch#name()})\ %h%m%r%=%y\ \ %-14(%{&sw}:%{&sts}:%{&ts}%)%-14.(%l,%c%V%)\ %P
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
set spelllang=en_us,en_gb
set completeopt=menu
set shell=/bin/sh
set signcolumn=yes

" Copy all yanked text to system clipboard (requires `xclip`)
" set clipboard+=unnamedplus

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

let g:signify_vcs_list = ['git']

" Markdown
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_no_default_key_mappings = 1

" Latex
let g:vimtex_quickfix_mode = 0

" Svelte
let g:svelte_preprocessors = ['typescript', 'ts']
let g:svelte_preprocessor_tags = [
  \ { 'name': 'ts', 'tag': 'script', 'as': 'typescript' }
  \ ]

" inccommand
if has("nvim")
  set inccommand=nosplit
endif

" Save on focus lost
au FocusLost * call s:SaveOnFocusLost()
function! s:SaveOnFocusLost()
  if !empty(expand('%:p')) && &modified
    write
  endif
endfunction

" Per file-type indentation
au FileType haskell     setlocal number sts=4 sw=4 expandtab formatprg=stylish-haskell
au FileType javascript  setlocal number sts=2 sw=2 expandtab nowrap
au FileType typescript  setlocal number sts=2 sw=2 expandtab nowrap
au FileType svelte      setlocal number sts=2 sw=2 expandtab nowrap
au FileType css         setlocal number sts=2 sw=2 expandtab nowrap
au FileType go          setlocal number ts=4  sw=4 noexpandtab
au FileType c,cpp,glsl  setlocal number ts=4  sw=4 noexpandtab
au FileType lua         setlocal number       sw=2 expandtab
au FileType sh,zsh      setlocal number sts=2 sw=2 expandtab
au FileType vim,ruby    setlocal number sts=2 sw=2 expandtab
au FileType help        setlocal number ts=4  sw=4 noexpandtab
au FileType solidity    setlocal number ts=4  sw=4 expandtab nowrap
au FileType graphql     setlocal number ts=4  sw=4 expandtab nowrap
au FileType rust        setlocal number signcolumn=yes nowrap colorcolumn=100 textwidth=100
au FileType plain       setlocal nonumber noai nocin nosi inde= wrap linebreak textwidth=80
au FileType pandoc      setlocal nonumber
au FileType markdown    setlocal nonumber conceallevel=0
au FileType rst         setlocal nonumber sw=2 expandtab wrap linebreak textwidth=80
au FileType todo        setlocal nonumber sw=2 expandtab nolinebreak nowrap textwidth=0
au FileType fountain    setlocal nonumber noai nocin nosi inde= wrap linebreak
au FileType tex         setlocal

au BufRead,BufNewFile *.md        setf markdown
au BufRead,BufNewFile *.tex       setf tex
au BufRead,BufNewFile *.todo      setf todo
au BufRead,BufNewFile *.tikz      setf tex
au BufRead,BufNewFile *.toml      setf toml
au BufRead,BufNewFile *.rs        setf rust
au BufRead,BufNewFile *.mustache  setf mustache
au BufRead,BufNewFile *.tera      setf htmldjango
au BufRead,BufNewFile *.svelte    setf svelte

" If no file-type is detected, set to plain.
autocmd BufEnter * if &filetype == "" | setlocal ft=plain | endif

let c_no_curly_error = 1

if has("nvim")
  au TermOpen * set nonumber modifiable
endif

" Remove trailing whitespace on save
autocmd BufWritePre * call s:StripTrailing()
function! s:StripTrailing()
  if &ft =~ 'rust'
    return
  endif

  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfunction

" Markdown, highlight YAML frontmatter
let g:vim_markdown_frontmatter = 1

" We use a POSIX shell
let g:is_posix = 1

" Haskell
let g:haskellmode_completion_ghc = 0
let g:haskell_enable_quantification = 1
au FileType haskell setlocal omnifunc=necoghc#omnifunc
au FileType haskell setlocal makeprg=stack\ build\ --fast
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

if executable('ctags')
  au BufWritePost *.c,*.cc,*.cpp  silent !ctags -f .tags -R .
  au BufWritePost *.h             silent !ctags -f .tags -R .
endif

" File-type
filetype on
filetype plugin on
filetype indent on

nnoremap <Space> <NOP>
nnoremap Q       <NOP>

" Make `Y` behave like `D` and `C`
nnoremap Y       y$

" Copy selected text to clipboard
xnoremap Y       "+y

" Paste form clipboard
noremap PP       "+p

" Easy command mode switch
inoremap kj <Esc>
inoremap <C-l> <C-x><C-l>

" Correct spelling mistakes
inoremap <C-s> <c-g>u<Esc>[s1z=`]a<c-g>u

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
nnoremap <C-p>           #N
nnoremap c*              *Ncgn
nnoremap <Leader>h       :nohl<CR>

nnoremap <Leader>n      :cnext<CR>
nnoremap <Leader>p      :cprev<CR>

autocmd BufRead fugitive\:* xnoremap <buffer> dp :diffput<CR>
autocmd BufRead fugitive\:* xnoremap <buffer> do :diffget<CR>

" Support jsonc comments in json files
autocmd FileType json syntax match Comment +\/\/.\+$+

 " Select recently pasted text
nnoremap <leader>v       V`]

" Switch buffers easily
nnoremap <Tab>   <C-^>

" Switch between .c and .h files easily
autocmd BufRead,BufNewFile *.c,*.h nnoremap <silent> <S-Tab> :e %:p:s,.h$,.X123X,:s,.c$,.h,:s,.X123X$,.c,<CR>

" Switch between .cc and .h files easily
autocmd BufRead,BufNewFile *.cc,*.h nnoremap <silent> <S-Tab> :e %:p:s,.h$,.X123X,:s,.cc$,.h,:s,.X123X$,.cc,<CR>

" Actually easier to type and I do it by mistake anyway
cnoreabbrev W w
cnoreabbrev Q q

" Ack
cnoreabbrev ack Ack!

" File navigation/search
nnoremap <Leader>o      :FuzzyOpen<CR>
nnoremap <Leader>f      :FuzzyGrep<CR>
nnoremap <Leader>t      :FuzzyTodo<CR>

" Navigate relative to the current file
cmap     %/         %:p:h/

map <Leader>m       :make<CR>
map <Leader>e       :e ~/.vimrc<CR>
map <Leader>s       :source ~/.vimrc<CR>

" Repeat previous command
map <Leader><Space> @:

" Commenting
nmap <C-/>           <Plug>CommentaryLine
xmap <C-/>           <Plug>Commentary

if has("nvim")
  tnoremap <Esc> <C-\><C-n>
endif

if executable('rg')
  let g:ackprg = 'rg -F -S --no-heading --vimgrep'
  set grepprg=rg\ -S\ -F\ --no-heading\ --vimgrep\ $*
endif

" Syntax coloring
syntax enable

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
command! Syn call s:Syn()
function! s:Syn()
  echo synIDattr(synID(line("."), col("."), 1), "name")
endfunction

if has("nvim")
  call plug#begin()

  Plug 'tpope/vim-commentary'
  Plug 'evanleck/vim-svelte', { 'for': ['svelte'], 'branch': 'main' }
  Plug 'pangloss/vim-javascript', { 'for': ['javascript'] }
  Plug 'mileszs/ack.vim'
  Plug 'bronson/vim-visual-star-search'
  Plug 'cloudhead/neovim-fuzzy'
  Plug 'cloudhead/shady.vim'
  Plug 'preservim/vim-markdown'
  Plug 'tikhomirov/vim-glsl'
  Plug 'junegunn/goyo.vim'
  Plug 'exu/pgsql.vim'
  Plug 'hail2u/vim-css3-syntax'
  Plug 'lervag/vimtex', { 'for': ['tex'] }
  Plug 'itchyny/vim-gitbranch'
  Plug 'cespare/vim-toml'
  Plug 'rust-lang/rust.vim'
  Plug 'neoclide/coc.nvim', { 'branch': 'release', 'for': ['rust', 'typescript', 'svelte'] }
  Plug 'tomlion/vim-solidity'
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'lewis6991/gitsigns.nvim'
  Plug 'leafgarland/typescript-vim', { 'for': ['typescript'] }
  Plug 'jparise/vim-graphql'

  call plug#end()
endif

"
" Quickfix Signs
"
sign define quickfix-error text=× texthl=ErrorSign

command! QuickfixSigns call s:QuickfixSigns()

" autocmd BufWrite * sign unplace *
autocmd CursorHold *.rs silent QuickfixSigns

function! s:QuickfixSigns()
  silent! cgetfile
  sign unplace *
  for dict in getqflist()
    if dict.type != 'E'
      continue
    endif
    try
      silent exe "sign"
          \ "place"
          \ dict.lnum
          \ "line=" . string(dict.lnum)
          \ "name=" . "quickfix-error"
          \ "file=" . bufname(dict.bufnr)
    catch

    endtry
  endfor
endfunction

command! Write setlocal spell   | Goyo 100x98%
command! Code  setlocal nospell | Goyo!
command! GitAdd silent !git add %

" Delete the current file.
command! Delete call delete(expand('%')) | bdelete!

if has("nvim")
  " Make sure we dont' load the rust cargo plugin from rust.vim!
  let g:loaded_rust_vim_plugin_cargo = 1
  " Don't add errors to quickfix if rustfmt fails.
  let g:rustfmt_fail_silently = 1
endif

" coc.vim
function! SetupCoc()
  nmap <silent> gd           <Plug>(coc-definition)
  nmap <silent> gy           <Plug>(coc-type-definition)
  nmap <silent> gi           <Plug>(coc-implementation)
  nmap <silent> gr           <Plug>(coc-references)
  nmap <silent> gj           <Plug>(coc-diagnostic-next)
  nmap <silent> gk           <Plug>(coc-diagnostic-prev)
  nmap <silent> <leader>/    :CocList --interactive symbols<CR>
  nmap <silent> <leader>r    <Plug>(coc-rename)

  inoremap <expr> <cr> coc#pum#visible() ? coc#_select_confirm() : "\<CR>"
endfunction

autocmd User CocNvimInit call SetupCoc()
command! CocStop call coc#rpc#stop()


" Use custom colors.
" This has to go after plugin initialization.
try
  colorscheme shady
catch
endtry
