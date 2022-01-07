"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2022-01-07 19:15:43 (CET)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" vim-plug
"" https://github.com/junegunn/vim-plug
"" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')
"
Plug 'w0rp/ale'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'vim-syntastic/syntastic'
Plug 'majutsushi/tagbar'
Plug 'vimwiki/vimwiki'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'chrisbra/vim-diff-enhanced'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-buffer.vim'
"
Plug 'vim-scripts/bash-support.vim'
Plug 'vim-scripts/c.vim'
Plug 'fatih/vim-go'
Plug 'vim-perl/vim-perl'
Plug 'neovimhaskell/haskell-vim'
"
Plug 'chriskempson/base16-vim'
Plug 'lifepillar/vim-solarized8'
Plug 'arcticicestudio/nord-vim'
Plug 'crusoexia/vim-monokai'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'sainnhe/gruvbox-material'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sainnhe/everforest'
Plug 'srcery-colors/srcery-vim'
Plug 'romgrk/doom-one.vim'
Plug 'raphamorim/lucario'
Plug 'miyakogi/seiya.vim'
"
"" List ends here. Plugins become visible to Vim after this call.
call plug#end()

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
source ~/.config/nvim/init.common.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
