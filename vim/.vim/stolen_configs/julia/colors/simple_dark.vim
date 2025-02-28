highlight clear
if exists("syntax on")
	syntax reset
endif

let g:colors_name="simple_dark"
set background=dark

#! this is a comment
highlight Normal ctermfg=229 ctermbg=234
highlight Comment ctermfg=245
highlight Constant ctermfg=110
highlight String ctermfg=70
highlight Character ctermfg=70
highlight Statement ctermfg=178
highlight PreProc ctermfg=31
highlight Type ctermfg=167
highlight Error ctermfg=235
highlight Identifier ctermfg=235
highlight Special ctermfg=172 cterm=bold

highlight ColorColumn ctermfg=196 ctermbg=235 cterm=bold
highlight LineNr ctermfg=243 ctermbg=255
highlight TabLine ctermfg=235 ctermbg=252 cterm=NONE
highlight TabLineFill ctermfg=235 ctermbg=252 cterm=NONE
highlight StatusLine ctermfg=252 ctermbg=235
highlight StatusLineNC ctermfg=252 ctermbg=243
highlight VertSplit ctermfg=254 ctermbg=252

highlight Visual ctermfg=235 ctermbg=153
highlight Pmenu ctermfg=235 ctermbg=153
highlight PmenuSel ctermfg=235 ctermbg=255
