"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:18:50 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" flattened
"" colorscheme flattened_light

"" vim-solarized8
"" https://github.com/lifepillar/vim-solarized8
""
let g:solarized_visibility = "normal"
let g:solarized_diffmode = "normal"
let g:solarized_italics = 0
set t_Co=256
set background=dark
" colorscheme solarized8
" colorscheme solarized8_low
colorscheme solarized8_high
" colorscheme solarized8_flat
if (has("termguicolors"))
    set termguicolors
endif

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'solarized',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
