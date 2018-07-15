" Vim syntax file
"
" Modification of vims Haskell syntax file:
"   - match types using regular expression
"   - use "syntax keyword" instead of "syntax match" where appropriate
"   - functions and types in import and module declarations are matched
"   - removed hs_highlight_more_types (just not needed anymore)
"   - enable spell checking in comments and strings only
"   - FFI highlighting
"   - QuasiQuotation
"   - top level Template Haskell slices
"   - PackageImport
"
" From Original file:
" ===================
"
" Language:		    Haskell
" Maintainer:		Haskell Cafe mailinglist <haskell-cafe@haskell.org>
" Last Change:		2010 Feb 21
" Original Author:	John Williams <jrw@pobox.com>
"
" Thanks to Ryan Crumley for suggestions and John Meacham for
" pointing out bugs. Also thanks to Ian Lynagh and Donald Bruce Stewart
" for providing the inspiration for the inclusion of the handling
" of C preprocessor directives, and for pointing out a bug in the
" end-of-line comment handling.
"
" Options-assign a value to these variables to turn the option on:
"
" hs_allow_hash_operator - Don't highlight seemingly incorrect C
"			   preprocessor directives but assume them to be
"			   operators
"

if exists("b:current_syntax")
  finish
endif

"syntax sync fromstart "mmhhhh.... is this really ok to do so?
syntax sync linebreaks=15 minlines=50 maxlines=500

syn match  hsSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match  hsSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match  hsSpecialCharError	contained "\\&\|'''\+"
sy region  hsString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=hsSpecialChar,@Spell
sy match   hsCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=hsSpecialChar,hsSpecialCharError
sy match   hsCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=hsSpecialChar,hsSpecialCharError
sy match   hsOpFunctionName "(\(\W\&[^(),\"]\)\+)" contained
sy match   hsHlFunctionName "[a-z_]\(\S\&[^,\(\)\[\]]\)*" contained



" (Qualified) identifiers (no default highlighting)
syn match ConId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[A-Z][a-zA-Z0-9_']*\>"
syn match VarId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_']*\>"

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match hsVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match hsConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match hsVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match hsConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

" Reserved symbols--cannot be overloaded.
syn match hsDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"

sy region hsInnerParen start="(" end=")" contained contains=hsInnerParen,hsConSym,hsType,hsVarSym

sy keyword hsStructure data family class where instance default deriving
sy keyword hsTypedef type newtype
sy keyword hsPattern pattern

sy keyword hsInfix infix infixl infixr
sy keyword hsStatement  do case of let in
sy keyword hsConditional if then else

" Primitive types from the standard prelude and libraries.
sy match hsType "\<[A-Z]\(\S\&[^,.{}]\)*\>"
sy match hsType "()"

syn keyword hsBoolean True False

syn region	hsPackageString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained
sy match   hsModuleName  excludenl "\([A-Z]\w*\.\?\)*" contained

sy match hsImport "\<import\>\s\+\(qualified\s\+\)\?\(\<\(\w\|\.\)*\>\)\s*\(()\)\?"
    \ contains=hsModuleName,hsImportLabel
    \ nextgroup=hsImportParams,hsImportIllegal skipwhite
sy keyword hsImportLabel import qualified contained

sy match hsImportIllegal "\w\+" contained

sy keyword hsAsLabel as contained
sy keyword hsHidingLabel hiding contained

sy match hsImportParams "as\s\+\(\w\+\)" contained
    \ contains=hsModuleName,hsAsLabel
    \ nextgroup=hsImportParams,hsImportIllegal skipwhite
sy match hsImportParams "hiding" contained
    \ contains=hsHidingLabel
    \ nextgroup=hsImportParams,hsImportIllegal skipwhite
sy region hsImportParams start="(" end=")" contained
    \ contains=hsBlockComment,hsLineComment, hsType,hsDelimTypeExport,hsHlFunctionName,hsOpFunctionName
    \ nextgroup=hsImportIllegal skipwhite

" new module highlighting
syn region hsDelimTypeExport start="\<[A-Z]\(\S\&[^,.]\)*\>(" end=")" contained
   \ contains=hsType

sy keyword hsExportModuleLabel module contained
sy match hsExportModule "\<module\>\(\s\|\t\|\n\)*\([A-Z]\w*\.\?\)*" contained contains=hsExportModuleLabel,hsModuleName

sy keyword hsModuleStartLabel module contained
sy keyword hsModuleWhereLabel where contained

syn match hsModuleStart "^module\(\s\|\n\)*\(\<\(\w\|\.\)*\>\)\(\s\|\n\)*"
  \ contains=hsModuleStartLabel,hsModuleName
  \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel

syn region hsModuleCommentA start="{-" end="-}"
  \ contains=hsModuleCommentA,hsCommentTodo,@Spell contained
  \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel skipwhite skipnl

syn match hsModuleCommentA "--.*\n"
  \ contains=hsCommentTodo,@Spell contained
  \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel skipwhite skipnl

syn region hsModuleExports start="(" end=")" contained
   \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl
   \ contains=hsBlockComment,hsLineComment,hsType,hsDelimTypeExport,hsHlFunctionName,hsOpFunctionName,hsExportModule,hsPattern

syn match hsModuleCommentB "--.*\n"
  \ contains=hsCommentTodo,@Spell contained
  \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl

syn region hsModuleCommentB start="{-" end="-}"
   \ contains=hsModuleCommentB,hsCommentTodo,@Spell contained
   \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl
" end module highlighting

" FFI support
sy keyword hsFFIForeign foreign contained
"sy keyword hsFFIImportExport import export contained
sy keyword hsFFIImportExport export contained
sy keyword hsFFICallConvention ccall stdcall contained
sy keyword hsFFISafety safe unsafe contained
sy region  hsFFIString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contained contains=hsSpecialChar
sy match hsFFI excludenl "\<foreign\>\(.\&[^\"]\)*\"\(.\)*\"\(\s\|\n\)*\(.\)*::"
  \ keepend
  \ contains=hsFFIForeign,hsFFIImportExport,hsFFICallConvention,hsFFISafety,hsFFIString,hsOpFunctionName,hsHlFunctionName


sy match   hsNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
sy match   hsFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" Comments
sy keyword hsCommentTodo    TODO FIXME XXX TBD contained
sy match   hsLineComment      "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=hsCommentTodo,@Spell
sy region  hsBlockComment     start="{-"  end="-}" contains=hsBlockComment,hsCommentTodo,@Spell
sy region  hsPragma	       start="{-#" end="#-}"

" QuasiQuotation
sy region hsQQ start="\[\$" end="|\]"me=e-2 keepend contains=hsQQVarID,hsQQContent nextgroup=hsQQEnd
sy region hsQQNew start="\[\(.\&[^|]\&\S\)*|" end="|\]"me=e-2 keepend contains=hsQQVarIDNew,hsQQContent nextgroup=hsQQEnd
sy match hsQQContent ".*" contained
sy match hsQQEnd "|\]" contained
sy match hsQQVarID "\[\$\(.\&[^|]\)*|" contained
sy match hsQQVarIDNew "\[\(.\&[^|]\)*|" contained

" Debugging functions from the standard prelude.
syn keyword hsDebug trace traceM traceShowM
syn keyword hsUndefined undefined error

" C Preprocessor directives. Shamelessly ripped from c.vim and trimmed
" First, see whether to flag directive-like lines or not
if (!exists("hs_allow_hash_operator"))
    syn match	cError		display "^\s*\(%:\|#\).*$"
endif

" Accept %: for # (C99)
syn region	cPreCondit	start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCommentError
syn match	cPreCondit	display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region	cCppOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
syn region	cCppOut2	contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cCppSkip
syn region	cCppSkip	contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cCppSkip
syn region	cIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	cIncluded	display contained "<[^>]*>"
syn match	cInclude	display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=cIncluded
syn cluster	cPreProcGroup	contains=cPreCondit,cIncluded,cInclude,cDefine,cCppOut,cCppOut2,cCppSkip,cCommentStartError
syn region	cDefine		matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$"
syn region	cPreProc	matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend

syn region	cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=cCommentStartError,cSpaceError contained
syntax match	cCommentError	display "\*/" contained
syntax match	cCommentStartError display "/\*"me=e-1 contained
syn region	cCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained

hi def link hsTypedef          Typedef
hi def link hsPattern          Keyword
hi def link hsVarSym           hsOperator
hi def link hsConSym           hsOperator
hi def link hsDelimiter        Delimiter

hi def link hsModuleStartLabel Structure
hi def link hsExportModuleLabel Keyword
hi def link hsModuleWhereLabel Structure
hi def link hsModuleName       Normal

hi def link hsImportIllegal    Error
hi def link hsAsLabel          hsImportLabel
hi def link hsHidingLabel      hsImportLabel
hi def link hsImportLabel      Include
hi def link hsImportMod        Include
hi def link hsPackageString    hsString

hi def link hsOperator         Operator

hi def link hsInfix            Keyword
hi def link hsStructure        Structure
hi def link hsStatement        Statement
hi def link hsConditional      Conditional

hi def link hsSpecialCharError Error
hi def link hsSpecialChar      SpecialChar
hi def link hsString           String
hi def link hsFFIString        String
hi def link hsCharacter        Character
hi def link hsNumber           Number
hi def link hsFloat            Float

hi def link hsLiterateComment		  hsComment
hi def link hsBlockComment     hsComment
hi def link hsLineComment      hsComment
hi def link hsModuleCommentA   hsComment
hi def link hsModuleCommentB   hsComment
hi def link hsComment          Comment
hi def link hsCommentTodo      Todo
hi def link hsPragma           SpecialComment
hi def link hsBoolean			  Boolean

hi def link hsDelimTypeExport  hsType
hi def link hsType             Type

hi def link hsDebug            Debug
hi def link hsUndefined        Keyword

hi def link cCppString         hsString
hi def link cCommentStart      hsComment
hi def link cCommentError      hsError
hi def link cCommentStartError hsError
hi def link cInclude           Include
hi def link cPreProc           PreProc
hi def link cDefine            Macro
hi def link cIncluded          hsString
hi def link cError             Error
hi def link cPreCondit         PreCondit
hi def link cComment           Comment
hi def link cCppSkip           cCppOut
hi def link cCppOut2           cCppOut
hi def link cCppOut            Comment

hi def link hsFFIForeign       Keyword
hi def link hsFFIImportExport  Structure
hi def link hsFFICallConvention Keyword
hi def link hsFFISafety         Keyword

hi def link hsTHIDTopLevel   Macro
hi def link hsTHTopLevelName Macro

hi def link hsQQVarID Keyword
hi def link hsQQVarIDNew Keyword
hi def link hsQQEnd   Keyword
hi def link hsQQContent String

let b:current_syntax = "haskell"
