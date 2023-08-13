"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:14:57 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" vim-monokai
"" https://github.com/crusoexia/vim-monokai
""
" let g:everforest_background = 'hard'
let g:everforest_background = 'medium'
" let g:everforest_background = 'soft'
let g:everforest_enable_italic = 0
let g:everforest_disable_italic_comment = 1
let g:everforest_show_eob = 0
let g:everforest_transparent_background = 0
set background=dark
syntax on
colorscheme everforest

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'everforest',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
