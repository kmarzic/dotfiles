"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:17:06 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" vim-monokai
"" https://github.com/crusoexia/vim-monokai
""
set background=dark
syntax on
colorscheme monokai

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'wombat',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
