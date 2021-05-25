call plug#begin('~/.local/share/nvim/plugged')
"improved syntax highlighting
" Plug 'sheerun/vim-polyglot' 
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'scrooloose/NERDTree'
Plug 'ryanoasis/vim-devicons'
Plug 'joshdick/onedark.vim'
Plug 'vimwiki/vimwiki'
Plug 'tomasiser/vim-code-dark'
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'christianchiarulli/nvcode-color-schemes.vim'

call plug#end()
