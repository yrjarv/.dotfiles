highlight clear
if exists("syntax on")
	syntax reset
endif

let g:colors_name="simple"
set background=light

highlight Normal ctermfg=235 ctermbg=255
highlight Comment ctermfg=243
highlight Constant ctermfg=26
highlight String ctermfg=64
highlight Character ctermfg=64
highlight Statement ctermfg=136
highlight PreProc ctermfg=31
highlight Type ctermfg=162
highlight Error ctermfg=235
highlight Identifier ctermfg=235
highlight Special ctermfg=136 cterm=bold
highlight Tabs ctermfg=254

highlight ColorColumn ctermfg=196 ctermbg=254 cterm=bold
highlight LineNr ctermfg=243 ctermbg=255
highlight TabLine ctermfg=235 ctermbg=252 cterm=NONE
highlight TabLineFill ctermfg=235 ctermbg=252 cterm=NONE
highlight StatusLine ctermfg=252 ctermbg=235
highlight StatusLineNC ctermfg=252 ctermbg=243
highlight VertSplit ctermfg=254 ctermbg=252

highlight Visual ctermfg=235 ctermbg=153
highlight Pmenu ctermfg=235 ctermbg=153
highlight PmenuSel ctermfg=235 ctermbg=255
