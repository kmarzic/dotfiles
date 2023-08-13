"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-08-13 11:17:23 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Common
source ~/.config/nvim/init.common0.vim

"" arcticicestudio/nord-vim
"" https://github.com/arcticicestudio/nord-vim
""
augroup nord-overrides
  autocmd!
  autocmd ColorScheme nord highlight Comment term=underline ctermfg=4 guifg=#81A1C1
  autocmd ColorScheme nord highlight LineNr term=underline ctermfg=4 guifg=#81A1C1
  autocmd ColorScheme nord highlight SpecialKey term=underline ctermfg=4 guifg=#81A1C1
  " autocmd ColorScheme nord highlight CursorLine cterm=NONE ctermbg=234 guibg=#1a1b23
augroup END

let g:nord_italic = 1
set t_Co=256
set background=dark
syntax on
colorscheme nord
if (has("termguicolors"))
    set termguicolors
endif
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" Transparency
" highlight Normal ctermbg=NONE guibg=NONE
" highlight NonText ctermbg=NONE guibg=NONE

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'nord',
    \ }

"" Common
source ~/.config/nvim/init.common1.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
