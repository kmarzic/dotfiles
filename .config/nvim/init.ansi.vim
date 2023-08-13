"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:12:26 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" Custom
""
set t_Co=16
" set t_Co=256
set background=dark
syntax on
" highlight CursorLine term=reverse cterm=reverse gui=reverse
" highlight CursorLine NONE
" highlight CursorLine ctermfg=black ctermbg=Cyan gui=reverse
hi CursorLine   term=bold cterm=bold guibg=white
hi TabLine      ctermfg=Black  ctermbg=Gray      cterm=NONE
hi TabLineFill  ctermfg=Black  ctermbg=Gray      cterm=NONE
hi TabLineSel   ctermfg=Black  ctermbg=Cyan      cterm=NONE
hi Visual       ctermfg=Black  ctermbg=Gray      cterm=NONE

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
