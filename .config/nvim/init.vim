"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2024-04-22 16:41:09 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" vim-solarized8
"" https://github.com/lifepillar/vim-solarized8
""
" let g:solarized_visibility = "low"
" let g:solarized_visibility = "normal"
let g:solarized_visibility = "high"
" let g:solarized_diffmode = "low"
" let g:solarized_diffmode = "normal"
let g:solarized_diffmode = "high"
let g:solarized_italics = 0
" let g:solarized_termtrans = 1
" let g:solarized_term_italics = 1
" let g:solarized_old_cursor_style = 1
" let g:solarized_extra_hi_groups = 1
" let g:solarized_use16 = 1
let g:solarized_extra_hi_groups = 1
set t_Co=256
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set t_Co=256
set background=light
syntax on
" colorscheme solarized8
" colorscheme solarized8_low
colorscheme solarized8_high
" colorscheme solarized8_flat

hi Comment cterm=NONE

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
