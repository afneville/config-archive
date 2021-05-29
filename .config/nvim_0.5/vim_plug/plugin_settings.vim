let g:NERDTreeMinimalUI = v:true
let g:ycm_semantic_triggers = {
	\   'python': [ 're!\w{2}' ]
	\ }
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:gruvbox_contrast_dark = "medium"
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
   \ quit | endif
let g:fzf_preview_window = ['up:40%:hidden', 'ctrl-/']
" let g:fzf_prefer_tmux = 1
