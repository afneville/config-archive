" plugins
source $HOME/.config/nvim/vim_plug/plugins.vim
source $HOME/.config/nvim/vim_plug/plugin_settings.vim

" completion and lsp
source $HOME/.config/nvim/vim_plug/lsp_config.vim
luafile $HOME/.config/nvim/vim_plug/compe_config.lua
luafile $HOME/.config/nvim/vim_plug/python_lsp.lua
luafile $HOME/.config/nvim/vim_plug/treesitter_config.lua
luafile $HOME/.config/nvim/vim_plug/treesitter_highlighting.lua

" general vim script
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/netrw_settings.vim
source $HOME/.config/nvim/keys/bindings.vim

" colorschemes 
" source $HOME/.config/nvim/themes/onedark.vim
" source $HOME/.config/nvim/themes/doom_one.vim
colorscheme gruvbox

au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
