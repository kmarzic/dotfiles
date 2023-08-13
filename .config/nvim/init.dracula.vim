"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:14:38 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" Dracula
"" https://draculatheme.com/vim
""
augroup dracula_customization
    au!
    hi DraculaComment ctermfg=141
    " highlight CursorLine cterm=NONE ctermbg=234 guibg=#1a1b23
augroup END

let g:dracula_colorterm = 0
let g:dracula_italic = 0
set t_Co=256
set background=dark
syntax enable
colorscheme dracula
if (has("termguicolors"))
    set termguicolors
endif
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'powerlineish',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
