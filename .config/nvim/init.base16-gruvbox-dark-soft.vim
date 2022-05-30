"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2022-05-30 09:09:22 (CEST)
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
Plug 'miyakogi/seiya.vim'
"
"" List ends here. Plugins become visible to Vim after this call.
call plug#end()

"" base16-vim
"" https://github.com/chriskempson/base16-vim.git
""
let base16colorspace=256  " Access colors present in 256 colorspace
set t_Co=256
set background=dark
# set background=light
syntax on
colorscheme base16-gruvbox-dark-soft

"" lightline
"" https://github.com/itchyny/lightline.vim
""
let g:lightline = {
    \ 'colorscheme': 'powerline',
    \ }

"" Common
source ~/.config/nvim/init.common.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
