source $HOME/.config/nvim/vim_plug/plugins.vim
source $HOME/.config/nvim/vim_plug/plugin_settings.vim
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/netrw_settings.vim
source $HOME/.config/nvim/keys/bindings.vim
source $HOME/.config/nvim/themes/onedark.vim
"source $HOME/.config/nvim/themes/doom_one.vim
" colorscheme codedark

au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
"cmap y w !sudo tee %
