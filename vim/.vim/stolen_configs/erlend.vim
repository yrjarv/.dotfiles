set nocompatible
filetype plugin on
set colorcolumn=81
syntax on

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

augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost [^l]* call OpenQuickFixList()
augroup END

function OpenQuickFixList()
    wincmd o
    vert cwindow
    wincmd p
    wincmd =
endfunction

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
map <leader>, :NERDTreeToggle<CR>
nnoremap <C-L> :nohl<CR><C-L>
nnoremap <C-J> :bn<CR>
nnoremap <C-K> :bp<CR>
nnoremap <leader>æ :Explore<CR>
nnoremap <leader><Enter> :make<bar>copen<CR><CR>
nnoremap <leader><Tab> :w<CR>:only<CR>:make FILE=%<bar>copen<CR><CR><C-W>L<C-W>h
nnoremap <leader>c :ccl<CR>
nnoremap <leader>n :cn<CR>
nnoremap <leader>p :cp<CR>
nnoremap <leader>f :cnf<CR>
call plug#begin()
 Plug 'scrooloose/nerdtree'
 Plug 'ellisonleao/gruvbox.nvim'
 Plug 'lervag/vimtex'
call plug#end()
set background=dark
colorscheme gruvbox 
" Viewer options: One may configure the viewer either by specifying a built-in
" viewer method:
let g:vimtex_view_method = 'zathura'

" Or with a generic interface:
let g:vimtex_view_general_viewer = 'okular'
let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'

" VimTeX uses latexmk as the default compiler backend. If you use it, which is
" strongly recommended, you probably don't need to configure anything. If you
" want another compiler backend, you can change it as follows. The list of
" supported backends and further explanation is provided in the documentation,
" see ":help vimtex-compiler".
let g:vimtex_compiler_method = 'latexrun'

" Most VimTeX mappings rely on localleader and this can be changed with the
" following line. The default is usually fine and is the symbol "\".
let maplocalleader = ","

