"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2023-04-09 10:16:53 (CEST)
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
Plug 'catppuccin/nvim', { 'as': 'cattpuccin' }
Plug 'ianchanning/vim-selenized'
Plug 'miyakogi/seiya.vim'
"
"" List ends here. Plugins become visible to Vim after this call.
call plug#end()

"" cattpucin
"" https://github.com/catppuccin/vim
""
set t_Co=256
set background=dark
syntax on
" colorscheme catppuccin-frappe
colorscheme catppuccin-latte
" colorscheme catppuccin-macchiato
" colorscheme catppuccin-mocha
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'srcery',
    \ }

"" Common
source ~/.config/nvim/init.common.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------