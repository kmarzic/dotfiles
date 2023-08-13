"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:15:12 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" vim-monokai
"" https://github.com/crusoexia/vim-monokai
""
let g:everforest_background = 'hard'
" let g:everforest_background = 'medium'
" let g:everforest_background = 'soft'
let g:everforest_enable_italic = 0
let g:everforest_disable_italic_comment = 1
let g:everforest_show_eob = 0
let g:everforest_transparent_background = 0
set background=light
syntax on
colorscheme everforest
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

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
