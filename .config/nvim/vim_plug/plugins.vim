call plug#begin('~/.local/share/nvim/plugged')

" Better Syntax Support
Plug 'sheerun/vim-polyglot'
" File Explorer
Plug 'scrooloose/NERDTree'
" Auto pairs for '(' '[' '{'
Plug 'jiangmiao/auto-pairs'
" For language specific completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Colour Scheme
Plug 'joshdick/onedark.vim'
" Vimwiki
Plug 'vimwiki/vimwiki'
Plug 'tomasiser/vim-code-dark'

call plug#end()
