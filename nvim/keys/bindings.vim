let mapleader = " "
nnoremap <Space> <NOP>

" My custom actions with the leader key.

nnoremap <leader>t :Texplore<return>
nnoremap <leader>p :vs \| ter<return>A
nnoremap <leader>ft :Vex <CR>
nnoremap <leader>l :set number!<CR>
nnoremap <leader>r :set relativenumber!<CR>
nnoremap <leader>% :source %<CR>
inoremap ii <esc>
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l

inoremap <expr> <c-j> ("\<C-n>")
inoremap <expr> <c-k> ("\<C-p>")
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" show the next (previous) buffer on tab.
nnoremap <TAB> :bnext<CR>
nnoremap <S-TAB> :bprevious<CR>

" Keep the selection highlighted after indenting.

vnoremap < <gv
vnoremap > >gv

" Use the alt key to resize open windows.

nnoremap <M-j>    :resize -2<CR>
nnoremap <M-k>    :resize +2<CR>
nnoremap <M-h>    :vertical resize -2<CR>
nnoremap <M-l>    :vertical resize +2<CR>
