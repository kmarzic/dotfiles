"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-24 18:08:59 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" gruvbox
"" https://github.com/morhetz/gruvbox
""
set background=dark
syntax on
" let g:gruvbox_contrast_light = 'hard'
" let g:gruvbox_contrast_light = 'medium'
let g:gruvbox_contrast_light = 'soft'
let g:gruvbox_material_disable_italic_comment = 1
colorscheme gruvbox
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" gruvbox-material
"" https://github.com/sainnhe/gruvbox-material
""
" set background=dark
" syntax on
" " let g:gruvbox_material_background = 'hard'
" " let g:gruvbox_material_background = 'medium'
" let g:gruvbox_material_background = 'soft'
" let g:gruvbox_material_disable_italic_comment = 1
" colorscheme gruvbox-material
" if (has("termguicolors"))
"     " set termguicolors
" endif
" " let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" " let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" lightline
"" https://github.com/itchyny/lightline.vim
""
" let g:lightline = {
"     \ 'colorscheme': 'gruvbox',
"     \ }

let g:lightline = {
    \ 'colorscheme': 'powerline',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
