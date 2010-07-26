" cloudhead's color scheme


" ********************************************************************************
" The following are the preferred 16 colors for your terminal
"           Colors      Bright Colors
" Black     #4E4E4E     #7C7C7C
" Red       #FF6C60     #FFB6B0
" Green     #A8FF60     #CEFFAB
" Yellow    #FFFFB6     #FFFFCB
" Blue      #96CBFE     #FFFFCB
" Magenta   #FF73FD     #FF9CFE
" Cyan      #C6C5FE     #DFDFFE
" White     #EEEEEE     #FFFFFF
" ********************************************************************************
"
set background=dark
hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "cotingale"

" General colors
hi Normal               ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi NonText              ctermfg=black       ctermbg=NONE        cterm=NONE

hi Cursor               ctermfg=black       ctermbg=white       cterm=reverse
hi LineNr               ctermfg=236         ctermbg=NONE        cterm=NONE

hi VertSplit            ctermfg=236         ctermbg=NONE         cterm=NONE
hi StatusLine           ctermfg=238         ctermbg=NONE         cterm=NONE
hi StatusLineNC         ctermfg=236         ctermbg=NONE         cterm=NONE

hi Folded               ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi Title                ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi Visual               ctermfg=NONE        ctermbg=52          cterm=NONE

hi SpecialKey           ctermfg=NONE        ctermbg=NONE        cterm=NONE

hi WildMenu             ctermfg=black       ctermbg=yellow      cterm=NONE
hi PmenuSbar            ctermfg=black       ctermbg=white       cterm=NONE
"hi Ignore                                  ctermfg=NONE        ctermbg=NONE        cterm=NONE

hi Error                ctermfg=NONE        ctermbg=red         cterm=NONE
hi ErrorMsg             ctermfg=NONE        ctermbg=52          cterm=NONE
hi WarningMsg           ctermfg=NONE        ctermbg=172         cterm=NONE

" Message displayed in lower left, such as --INSERT--
hi ModeMsg              ctermfg=darkgrey    ctermbg=235         cterm=NONE

if version >= 700 " Vim 7.x specific colors
  hi CursorLine         ctermfg=NONE        ctermbg=NONE        cterm=NONE
  hi CursorColumn       ctermfg=NONE        ctermbg=NONE        cterm=BOLD
  hi TabLine            ctermfg=238         ctermbg=NONE        cterm=NONE
  hi TabLineFill        ctermfg=238         ctermbg=NONE        cterm=NONE
  hi TabLineSel         ctermfg=NONE        ctermbg=NONE        cterm=BOLD
  hi MatchParen         ctermfg=NONE        ctermbg=NONE        cterm=BOLD
  hi Pmenu              ctermfg=NONE        ctermbg=NONE        cterm=NONE
  hi PmenuSel           ctermfg=black       ctermbg=yellow      cterm=NONE
  hi Search             ctermfg=NONE        ctermbg=NONE        cterm=underline
endif

" Syntax highlighting
hi Comment              ctermfg=239         ctermbg=NONE        cterm=NONE
hi String               ctermfg=114         ctermbg=NONE        cterm=NONE
hi Number               ctermfg=139         ctermbg=NONE        cterm=NONE

hi Keyword              ctermfg=110         ctermbg=NONE        cterm=NONE
hi PreProc              ctermfg=cyan        ctermbg=NONE        cterm=NONE

hi Todo                 ctermfg=240         ctermbg=NONE        cterm=bold
hi Constant             ctermfg=180         ctermbg=NONE        cterm=NONE

hi Identifier           ctermfg=146         ctermbg=NONE        cterm=NONE
hi Function             ctermfg=103         ctermbg=NONE        cterm=NONE
hi Class                ctermfg=67          ctermbg=NONE        cterm=bold
hi Type                 ctermfg=yellow      ctermbg=NONE        cterm=NONE

hi Special              ctermfg=172         ctermbg=NONE        cterm=NONE
hi Delimiter            ctermfg=cyan        ctermbg=NONE        cterm=NONE
"hi Operator                              ctermfg=black       ctermbg=NONE        cterm=NONE

hi link Character       Constant
hi link Conditional     Keyword
hi link Statement       Keyword
hi link Boolean         Constant
hi link Float           Number
hi link Repeat          Statement
hi link Label           Statement
hi link Exception       Statement
hi link Include         PreProc
hi link Define          PreProc
hi link Macro           PreProc
hi link PreCondit       PreProc
hi link StorageClass    Type
hi link Structure       Type
hi link Typedef         Type
"hi link Tag             Special
hi link SpecialChar     Special
hi link SpecialComment  Special
hi link Debug           Special


" Special for Ruby
hi rubyRegexp                                    ctermfg=64         ctermbg=NONE      cterm=NONE
hi rubyRegexpDelimiter                           ctermfg=28         ctermbg=NONE      cterm=NONE
hi rubyFunction                                  ctermfg=179        ctermbg=NONE      cterm=NONE
hi rubyEscape                                    ctermfg=166        ctermbg=NONE      cterm=NONE
hi rubyInterpolationDelimiter                    ctermfg=30         ctermbg=NONE      cterm=NONE
hi rubyStringDelimiter                           ctermfg=114        ctermbg=NONE      cterm=NONE
hi rubySymbol                                    ctermfg=107        ctermbg=NONE      cterm=NONE
hi rubyConstant                                  ctermfg=186        ctermbg=NONE      cterm=NONE
hi rubyOperator                                  ctermfg=179        ctermbg=NONE      cterm=NONE
hi link rubyPseudoOperator rubyOperator
"hi rubyGlobalVariable                            ctermfg=lightblue      ctermbg=NONE      cterm=NONE  "yield
"rubySharpBang
"rubyPredefinedVariable
"rubyBoolean
"rubyClassVariable
"rubyBeginEnd
"rubyRepeatModifier
hi link rubyArrayDelimiter    Identifier  " [ , , ]
"rubyCurlyBlock  { , , }
hi link rubyControl           Keyword    " when
hi link rubyDefine            Keyword
hi link rubyAttribute         Keyword
hi link rubyInclude           Keyword    " include
hi link rubyModule            Keyword    " module
hi link rubyClass             Keyword    " class
hi link rubyKeyword           Keyword    " yield, super
hi link rubyAccess            Keyword    " private
hi link rubyEval              Special    " eval
hi link rubyMethod            Keyword    " def
hi link rubyIdentifier        Identifier " |var|
hi link rubyInstanceVariable  Identifier " @var
hi link rubyGlobalVariable    Identifier " $var
hi link rubyClassVariable     Identifier " @@var
hi link rubyPseudoVariable    Identifier " self, nil

hi link lessVariable          Keyword
" Special for XML
"hi link xmlTagName      Keyword
"hi link xmlTag          Identifier
""hi link xmlEndTag       Identifier
"
"" Special for HTML
""hi link htmlTagName     Keyword
"hi link htmlTag         Identifier
"hi link htmlEndTag      Identifier
"
"" Special for Javascript
hi link javaScriptNumber      Number

hi link javaScriptPrototype      Identifier " prototype
hi link javaScriptSource         Keyword " import export
hi link javaScriptType           Identifier " const this undefined var void yield 
hi link javaScriptOperator       Keyword " delete new in instanceof let typeof
"hi link javaScriptBoolean        Keyword " true false
"hi link javaScriptNull           Keyword " null
hi link javaScriptConditional    Keyword " if else
hi link javaScriptRepeat         Keyword " do while for
hi link javaScriptBranch         Keyword " break continue switch case default return
hi link javaScriptStatement      Keyword " try catch throw with finally
"hi link javaScriptGlobalObjects  Keyword " Array Boolean Date Function Infinity JavaArray JavaClass JavaObject JavaPackage Math Number NaN Object Packages RegExp String Undefined java netscape sun
hi shCommandSub		ctermfg=white
