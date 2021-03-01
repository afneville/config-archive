call plug#begin('~/.local/share/nvim/plugged') 
""Plug 'vim-airline/vim-airline'
""Plug 'vim-airline/vim-airline-themes'
Plug 'yuttie/comfortable-motion.vim'
Plug 'tomasiser/vim-code-dark'
Plug 'mattn/emmet-vim'
Plug 'jremmen/vim-ripgrep' 
"Plug 'ycm-core/YouCompleteMe'
Plug 'itchyny/lightline.vim'
Plug 'sonph/onehalf', { 'rtp': 'vim' }
"Plug 'rakr/vim-one'
Plug 'rafi/awesome-vim-colorschemes'
"Plug 'wadackel/vim-dogrun'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'phpactor/phpactor', {'for': 'php', 'branch': 'master', 'do': 'composer install --no-dev -o'}
Plug 'vimwiki/vimwiki'
Plug 'arzg/vim-colors-xcode'
Plug 'sainnhe/edge'
Plug 'preservim/nerdtree'
Plug 'sainnhe/sonokai'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'yuezk/vim-js'
Plug 'baskerville/bubblegum'
Plug 'romgrk/doom-one.vim'
Plug 'rakr/vim-one'
Plug 'arcticicestudio/nord-vim'
Plug 'dracula/vim', { 'as': 'dracula' }
"Plug 'jeaye/color_coded'

call plug#end()

" Plugin related stuff"
let g:NERDTreeMinimalUI = v:true
let g:lightline = {
    \ 'colorscheme': 'seoul256',
    \ }
let g:user_emmet_mode='a'
let g:user_emmet_leader_key='<C-Z>'
let mapleader = " "
""let g:netrw_browse_split=0
let g:netrw_banner=0
source $HOME/.config/nvim/plug-config/coc.vim
autocmd FileType vimwiki let b:coc_suggest_disable = 1
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif
let g:coc_global_extensions = [
    \ 'coc-snippets',
    \ 'coc-pairs',
    \ 'coc-tsserver',
    \ 'coc-prettier',
    \ 'coc-json',
    \ 'coc-clangd',
    \ 'coc-python',
  \ ]

"editor related stuff"
syntax on
filetype on
filetype plugin on
filetype indent on
set nobackup
set nowritebackup
set nocompatible
set noswapfile

" feature related stuff "
set noshowmode
set nowrap
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set hidden
set splitbelow
"set cursorline

" apperance 
set modelines=0
set laststatus=0
set t_Co=256
colorscheme edge
"for transparency:
"hi background=dark
highlight LineNr ctermfg=darkgrey
highlight VertSplit ctermbg=none
highlight VertSplit ctermfg=darkgrey
highlight MatchParen cterm=none ctermbg=darkgrey ctermfg=blue
"highlight TabLine ctermfg=none ctermbg=none

" keyboard and mouse "
set mouse=a
nnoremap <M-t> :Texplore<return>
nnoremap <C-p> :vs \| ter<return>A
nnoremap <leader>t :NERDTreeFocus<CR>
nnoremap <leader>f :NERDTreeToggle<CR>
nnoremap <leader>l :set number!<CR>
inoremap ii <esc>


