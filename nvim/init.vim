source $HOME/.config/nvim/vim-plug/plugins.vim
source $HOME/.config/nvim/vim-plug/plugin-settings.vim
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/keys/bindings.vim
source $HOME/.config/nvim/themes/onedark.vim

au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
cmap w!! w !sudo tee %
