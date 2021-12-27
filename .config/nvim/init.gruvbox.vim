"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2021-12-27 19:48:00 (CET)
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

"" gruvbox
"" https://github.com/morhetz/gruvbox
"" https://github.com/sainnhe/gruvbox-material
""
set background=dark
syntax on
" colorscheme gruvbox
""
" let g:gruvbox_material_background = 'soft'
" let g:gruvbox_material_background = 'medium'
let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_disable_italic_comment = 1
colorscheme gruvbox-material

"" lightline
"" https://github.com/itchyny/lightline.vim
""
" let g:lightline = {
"     \ 'colorscheme': 'gruvbox',
"     \ }

let g:lightline = {
        \ 'colorscheme': 'gruvbox-material',
        \ }

"" Common
source ~/.config/nvim/init.common.vim

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
