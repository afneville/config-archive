" leader key
let mapleader = " "
nnoremap <Space> <NOP>

autocmd! InsertEnter * norm zz
"cmap y w !sudo tee %

"line numbers
nnoremap <leader>tl :set number!<CR>
nnoremap <leader>tr :set relativenumber!<CR>
" misc
nnoremap <leader>ft :NERDTreeToggle <CR>
" nnoremap <leader>ft :Vex <CR>
nnoremap <leader>t :Texplore<return>
nnoremap <leader>p :vs \| ter<return>A
nnoremap <leader>% :source %<CR>
" switch buffers
nnoremap <TAB> :bnext<CR>
nnoremap <S-TAB> :bprevious<CR>
" insert lines above or below
nnoremap <leader>ij o<esc>0Dk
nnoremap <leader>ik O<esc>0Dj
" switch windows
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l
" window resizing
nnoremap <M-j>    :resize -2<CR>
nnoremap <M-k>    :resize +2<CR>
nnoremap <M-h>    :vertical resize -2<CR>
nnoremap <M-l>    :vertical resize +2<CR>
" mover between completions
inoremap <expr> <c-j> ("\<C-n>")
inoremap <expr> <c-k> ("\<C-p>")
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
" dismiss diagnosatics
nnoremap <leader>wmm :cclose<CR>
" fuzzy finding
nnoremap <leader>ff :Files<CR>
nnoremap <leader>ht :Colors<CR>
nnoremap <leader>sb :Lines<CR>
nnoremap / :Lines<CR>
nnoremap <leader>bs :Buffers<CR>
" latex
nnoremap <leader>ll :VimtexCompile<CR>
nnoremap <leader>lc :VimtexClean<CR>
nnoremap <leader>lv :VimtexView<CR>


" Keep the selection highlighted after indenting.
vnoremap < <gv
vnoremap > >gv
