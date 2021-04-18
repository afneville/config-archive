let g:NERDTreeMinimalUI = v:true
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif

source $HOME/.config/nvim/vim-plug/coc.vim

let g:coc_global_extensions = [
    \ 'coc-snippets',
    \ 'coc-pairs',
    \ 'coc-tsserver',
    \ 'coc-prettier',
    \ 'coc-json',
    \ 'coc-clangd',
    \ 'coc-python',
  \ ]

" Disable completion in vimwiki
autocmd FileType vimwiki let b:coc_suggest_disable = 1
