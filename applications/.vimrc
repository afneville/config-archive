" PLUGINS

call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'lervag/vimtex'
Plug 'SirVer/ultisnips'
Plug 'arcticicestudio/nord-vim'
Plug 'KeitaNakamura/tex-conceal.vim'

call plug#end()

" GENERAL CONFIG OPTIONS

syntax enable
filetype plugin on
filetype indent on
set autoread
au FocusGained,BufEnter * checktime
set wildmenu
set ruler 
set cmdheight=1
set hid
set backspace=eol,start,indent
set ignorecase
set smartcase
set hlsearch
set incsearch
set lazyredraw
set magic
set showmatch
set mat=2
set noerrorbells
set novisualbell
set t_vb=
set tm=500
set foldcolumn=0
set nobackup
set nowb
set noswapfile
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set lbr
set tw=500
set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set nu
set relativenumber

autocmd BufRead,BufNewFile *.tex set spelllang=en_gb
autocmd BufRead,BufNewFile *.tex setlocal spell

" STATUS LINE

set laststatus=2
set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ %=\ \ Line:\ %l\ \ Column:\ %c\ 

" COLORSCHEME

colorscheme nord
" MAPPINGS

let mapleader = " "
nmap <leader>w :w!<cr>
nmap <leader>q :wqa!<cr>
nmap <leader>n :nohl<cr>
nmap <leader>s :source $MYVIMRC<cr>
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u

" PLUGIN CONFIG OPTIONS

let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

let g:vimtex_view_method = 'zathura'
let g:tex_flavor='latex'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'
hi Conceal ctermbg=none
