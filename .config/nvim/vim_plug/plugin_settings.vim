let g:NERDTreeMinimalUI = v:true
let g:ycm_semantic_triggers = {
	\   'python': [ 're!\w{2}' ]
	\ }
let g:ycm_autoclose_preview_window_after_insertion = 1
" autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
"    \ quit | endif

" source $HOME/.config/nvim/vim_plug/coc.vim

 " let g:coc_global_extensions = [
      " "\ 'coc-snippets',
     " "\ 'coc-pairs',
     " "\ 'coc-tsserver',
     " "\ 'coc-prettier',
     " "\ 'coc-json',
     " "\ 'coc-clangd',
     " "\ 'coc-python',
 " \ ]

" Disable completion in vimwiki
" autocmd FileType vimwiki let b:coc_suggest_disable = 1
