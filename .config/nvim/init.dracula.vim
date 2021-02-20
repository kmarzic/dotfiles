"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2021-02-20 17:02:58 (CET)
"" Current file: ~/.config/nvim/init.vim
"" ------------------------------------------------------------------------

"" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')
"
Plug 'w0rp/ale'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'majutsushi/tagbar'
Plug 'vimwiki/vimwiki'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'chrisbra/vim-diff-enhanced'
"
Plug 'vim-scripts/bash-support.vim'
Plug 'vim-scripts/c.vim'
Plug 'fatih/vim-go'
Plug 'vim-perl/vim-perl'
Plug 'hdima/python-syntax'
Plug 'neovimhaskell/haskell-vim'
"
Plug 'chriskempson/base16-vim'
" Plug 'rakr/vim-one'
" Plug 'rakr/vim-colors-rakr'
" Plug 'jeffkreeftmeijer/vim-dim'
" Plug 'kyoz/purify'
" Plug 'joshdick/onedark.vim'
" Plug 'andreasvc/vim-256noir'
" Plug 'marcopaganini/termschool-vim-theme'
Plug 'romainl/flattened'
Plug 'lifepillar/vim-solarized8'
Plug 'arcticicestudio/nord-vim'
Plug 'crusoexia/vim-monokai'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'miyakogi/seiya.vim'
"
"" List ends here. Plugins become visible to Vim after this call.
call plug#end()

"" Dracula
"" https://draculatheme.com/vim
syntax enable
colorscheme dracula

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
