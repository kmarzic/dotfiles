"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2021-05-02 14:26:10 (CEST)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')
"
Plug 'w0rp/ale'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'majutsushi/tagbar'
Plug 'vimwiki/vimwiki'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'chrisbra/vim-diff-enhanced'
Plug 'maralla/completor.vim'
"
Plug 'vim-scripts/bash-support.vim'
Plug 'vim-scripts/c.vim'
Plug 'fatih/vim-go'
Plug 'vim-perl/vim-perl'
Plug 'hdima/python-syntax'
Plug 'neovimhaskell/haskell-vim'
"
Plug 'chriskempson/base16-vim'
Plug 'romainl/flattened'
Plug 'lifepillar/vim-solarized8'
Plug 'arcticicestudio/nord-vim'
Plug 'crusoexia/vim-monokai'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'NLKNguyen/papercolor-theme'
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

let g:lightline = {
    \ 'colorscheme': 'wombat',
    \ }

"" Common
source ~/.config/nvim/init.common.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
