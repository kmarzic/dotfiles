"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2024-05-03 19:29:00 (CEST)
"" Current file: ~/.config/nvim/init.common0.vim
"" ------------------------------------------------------------------------

"" ------------------------------------------------------------------------
"" Common
"" ------------------------------------------------------------------------

"" vim-plug
"" https://github.com/junegunn/vim-plug
""
"" Plugins will be downloaded under the specified directory.
""
call plug#begin('~/.local/share/nvim/site/plugged')
"
Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'vimwiki/vimwiki'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'chrisbra/vim-diff-enhanced'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-buffer.vim'
Plug 'dense-analysis/ale'
"
Plug 'WolfgangMehner/bash-support'
Plug 'WolfgangMehner/c-support'
Plug 'WolfgangMehner/perl-support'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'vim-perl/vim-perl'
Plug 'neovimhaskell/haskell-vim'
Plug 'rodjek/vim-puppet'
"
Plug 'tinted-theming/base16-vim'
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

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
