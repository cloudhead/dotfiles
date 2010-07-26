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

let colors_name = "cloudhead"

" General colors
hi Normal               ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi NonText              ctermfg=black       ctermbg=NONE        cterm=NONE

hi Cursor               ctermfg=black       ctermbg=white       cterm=reverse
hi LineNr               ctermfg=240         ctermbg=235         cterm=NONE

hi VertSplit            ctermfg=241         ctermbg=235         cterm=NONE
hi StatusLine           ctermfg=245         ctermbg=235         cterm=NONE
hi StatusLineNC         ctermfg=241         ctermbg=235         cterm=NONE

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
  hi CursorLine         ctermfg=NONE        ctermbg=235         cterm=NONE
  hi CursorColumn       ctermfg=NONE        ctermbg=NONE        cterm=BOLD
  hi MatchParen         ctermfg=NONE        ctermbg=black       cterm=BOLD
  hi Pmenu              ctermfg=NONE        ctermbg=NONE        cterm=NONE
  hi PmenuSel           ctermfg=black       ctermbg=yellow      cterm=NONE
  hi Search             ctermfg=NONE        ctermbg=NONE        cterm=underline
endif

" Syntax highlighting
hi Comment              ctermfg=239         ctermbg=NONE        cterm=NONE
hi String               ctermfg=106         ctermbg=NONE        cterm=NONE
hi Number               ctermfg=red         ctermbg=NONE        cterm=NONE

hi Keyword              ctermfg=228         ctermbg=NONE        cterm=NONE
hi PreProc              ctermfg=cyan        ctermbg=NONE        cterm=NONE

hi Todo                 ctermfg=240         ctermbg=NONE        cterm=bold
hi Constant             ctermfg=yellow      ctermbg=NONE        cterm=NONE

hi Identifier           ctermfg=cyan        ctermbg=NONE        cterm=NONE
hi Function             ctermfg=103         ctermbg=NONE        cterm=NONE
hi Class                ctermfg=67          ctermbg=NONE        cterm=bold
hi Type                 ctermfg=67          ctermbg=NONE        cterm=NONE

hi Special              ctermfg=166         ctermbg=NONE        cterm=NONE
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
hi rubyEscape                                    ctermfg=166        ctermbg=NONE      cterm=NONE
hi rubyInterpolationDelimiter                    ctermfg=red        ctermbg=NONE      cterm=NONE
hi rubyStringDelimiter                           ctermfg=28         ctermbg=NONE      cterm=NONE
hi rubySymbol                                    ctermfg=172        ctermbg=235      cterm=NONE
hi rubyConstant                                  ctermfg=66         ctermbg=NONE      cterm=NONE
hi rubyOperator                                  ctermfg=192        ctermbg=NONE      cterm=NONE
hi link rubyPseudoOperator rubyOperator
"hi rubyGlobalVariable                            ctermfg=lightblue      ctermbg=NONE      cterm=NONE  "yield
"rubySharpBang
"rubyAccess
"rubyPredefinedVariable
"rubyBoolean
"rubyClassVariable
"rubyBeginEnd
"rubyRepeatModifier
"hi link rubyArrayDelimiter    Special  " [ , , ]
"rubyCurlyBlock  { , , }
hi link rubyControl           Keyword
hi link rubyDefine            Keyword
hi link rubyInclude           Keyword
hi link rubyModule            Keyword
hi link rubyClass             Keyword
hi link rubyKeyword           Keyword
hi link rubyMethod            Keyword
hi link rubyOperator          Operator
hi link rubyIdentifier        Identifier
hi link rubyInstanceVariable  Identifier
hi link rubyGlobalVariable    Identifier
hi link rubyClassVariable     Identifier
hi link rubyConstant          Type

" Special for XML
hi link xmlTag          Keyword
hi link xmlTagName      Conditional
hi link xmlEndTag       Identifier

" Special for HTML
hi htmlTagName                            ctermfg=blue          ctermbg=NONE      cterm=bold
hi link htmlTag         Identifier
hi link htmlEndTag      Identifier

" Special for Javascript
hi link javaScriptNumber      Number

