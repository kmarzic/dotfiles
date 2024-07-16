"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2024-07-16 19:34:09 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" base16-vim
"" https://github.com/tinted-theming/base16-vim.git
""
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256
" set background=dark
set background=light
syntax on
colorscheme base16-atelier-savanna-light
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'powerline',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
