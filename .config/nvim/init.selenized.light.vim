"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:18:10 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" selenized-light
"" https://github.com/ianchanning/vim-selenized
""
set t_Co=256
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" set background=dark
set background=light
syntax on
colorscheme selenized
hi Comment gui=NONE cterm=NONE

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'selenized_light',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
