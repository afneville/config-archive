call plug#begin('~/.local/share/nvim/plugged') 

Plug 'yuttie/comfortable-motion.vim'
Plug 'tomasiser/vim-code-dark'
Plug 'mattn/emmet-vim'
Plug 'jremmen/vim-ripgrep' 
Plug 'itchyny/lightline.vim'
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'rafi/awesome-vim-colorschemes'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
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
Plug 'liuchengxu/space-vim-dark'
Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
"Plug 'vim-python/python-syntax'
"Plug 'ryanoasis/vim-devicons'

call plug#end()

" Plugin related stuff"
let g:NERDTreeMinimalUI = v:true
let g:lightline = {
    \ 'colorscheme': 'jellybeans',
    \ }
let g:user_emmet_mode='a'
let g:user_emmet_leader_key='<C-Z>'
let mapleader = " "
let g:netrw_browse_split=0
let g:netrw_banner=0
source $HOME/.config/nvim/coc.vim
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
set laststatus=2
set t_Co=256
colorscheme codedark
highlight LineNr ctermfg=darkgrey
highlight VertSplit ctermbg=none
highlight VertSplit ctermfg=darkgrey
highlight MatchParen cterm=none ctermbg=darkgrey ctermfg=blue

" keyboard and mouse "
set mouse=a
nnoremap <leader>t :Texplore<return>
nnoremap <leader>p :vs \| ter<return>A
nnoremap <leader>ff :NERDTreeFocus<CR>
nnoremap <leader>ft :NERDTreeToggle<CR>
nnoremap <leader>l :set number!<CR>
nnoremap <leader>r :set relativenumber!<CR>
nnoremap <leader>% :source %<CR>
inoremap ii <esc>
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l
