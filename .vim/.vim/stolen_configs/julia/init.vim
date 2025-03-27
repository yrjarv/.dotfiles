set nocompatible

set background=dark
set colorcolumn=81
set mouse=
syntax on

colorscheme gruvbox

set confirm
set wildmenu
set hlsearch
set ignorecase
set ruler
set number
set wrap

set showmode
set showcmd

filetype indent plugin on
set cindent
set cino=0{,0},0),0(,:,0#,!^F,o,O,e,

set backspace=indent,eol,start
set laststatus=2

set shiftwidth=4
set tabstop=4
set softtabstop=4
set noexpandtab

set listchars=tab:\│\ 

let whitelist = ['c', 'h', 'cpp', 'hpp', 'py']
autocmd BufWritePre * if index(whitelist, &ft) >= 0 | :%s/\s\+$//e

set notimeout ttimeout ttimeoutlen=200
set scrolloff=0
set encoding=utf-8
set formatoptions=ql
set formatoptions-=cro

au FileType c,cpp setlocal comments-=:// comments+=f://

let g:netrw_liststyle = 3
let g:netrw_browse_split = 0
let g:netrw_altv = 1
let g:netrw_banner = 1

map <space> <leader>
nnoremap <C-L> :nohl<CR><C-L>
nnoremap <leader>j :bn<CR>
nnoremap <leader>k :bp<CR>
nnoremap <leader><Tab> :tabn<CR>
nnoremap <leader><S-Tab> :tabp<CR>
nnoremap <leader>; :Explore<CR>
nnoremap <leader><Enter> :wa<CR>:make<bar>copen<CR><CR>
nnoremap <leader>t :wa<CR>:make todo<bar>copen<CR><CR>
nnoremap <leader>n :cn<CR>
nnoremap <leader>p :cp<CR>
nnoremap <leader>f :cnf<CR>
nnoremap <leader>c :ccl<CR>
nnoremap <leader>> :set colorcolumn=121<CR>
nnoremap <leader>< :set colorcolumn=81<CR>
nnoremap <leader>1 :set background=dark<CR>
nnoremap <leader>2 :set background=light<CR>

let c_no_curly_error=1
