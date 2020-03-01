"" ------------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2020-03-01 09:49:22 (CET)
"" Current file: ~/.config/nvim/init.vim
"" init.vim
"" ------------------------------------------------------------------------

"" ------------------------------------------------------------------------
"" Plugins
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
"
Plug 'vim-scripts/bash-support.vim'
Plug 'vim-scripts/c.vim'
Plug 'fatih/vim-go'
Plug 'vim-perl/vim-perl'
Plug 'hdima/python-syntax'
Plug 'neovimhaskell/haskell-vim'
"
" Plug 'chriskempson/base16-vim'
" Plug 'rakr/vim-one'
" Plug 'rakr/vim-colors-rakr'
" Plug 'jeffkreeftmeijer/vim-dim'
" Plug 'kyoz/purify'
" Plug 'joshdick/onedark.vim'
" Plug 'andreasvc/vim-256noir'
" Plug 'marcopaganini/termschool-vim-theme'
" Plug 'lifepillar/vim-solarized8'
Plug 'sainnhe/edge'
Plug 'miyakogi/seiya.vim'
"
"" List ends here. Plugins become visible to Vim after this call.
call plug#end()

"" ale
"" https://github.com/w0rp/ale
""
let b:ale_linters = ['pyflakes', 'flake8', 'pylint']
let g:ale_completion_enabled = 1
let b:ale_fixers = ['eslint']
let b:ale_fix_on_save = 1

"" ctrlp.vim
"" https://github.com/ctrlpvim/ctrlp.vim
""
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0

"" fzf
"" https://github.com/junegunn/fzf.vim
""
" let g:fzf_action = {
"   \ 'ctrl-t': 'tab split',
"   \ 'ctrl-x': 'split',
"   \ 'ctrl-v': 'vsplit' }
" let g:fzf_layout = { 'down': '~40%' }

"" gitgutter
"" https://github.com/airblade/vim-gitgutter

"" lightline
"" https://github.com/itchyny/lightline.vim
""
" let g:lightline = {
"       \ 'colorscheme': 'solarized',
"       \ }

"" The NERD tree
"" https://github.com/scrooloose/nerdtree
""
let NERDTreeShowHidden = 1
let NERDTreeShowBookmarks = 1
let NERDTreeQuitOnOpen = 0
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 0
let NERDTreeDirArrows = 1
if has ("unix")
    let NERDTreeBookmarksFile=expand("$HOME/.vim/.vim-NERDTreeBookmarks")
else
    " let NERDTreeBookmarksFile=expand("$VIM/.vim-NERDTreeBookmarks")
endif
let g:NERDTreeWinSize = 50
" autocmd VimEnter * NERDTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map ,n :NERDTreeToggle<CR>
vmap ,n <esc>:NERDTreeToggle<CR>
imap ,n <esc>:NERDTreeToggle<CR>
" au BufWinEnter * set number

"" syntastic
"" https://github.com/scrooloose/syntastic
""
" let g:syntastic_disabled_filetypes=['html']
let g:syntastic_html_checkers=['']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

"" tagbar
"" https://github.com/majutsushi/tagbar
""
nmap <silent><F8> :TagbarToggle<CR>
nmap ,lt0 :TagbarToggle<CR>

"" vimwiki
"" https://github.com/vimwiki/vimwiki

"" --------------------------------------------------------------------

"" Bash-support
"" https://github.com/vim-scripts/bash-support.vim

"" c.vim
"" https://github.com/vim-scripts/c.vim

"" vim-go
"" https://github.com/faith/vim-go
""
" au BufNewFile,BufRead *.go set filetype=go
au BufNewFile,BufReadPost *.go set filetype=go

"" perl-vim
"" https://github.com/vim-perl/vim-perl

"" python-syntax
"" https://github.com/hdima/python-syntax

"" haskell-vim
"" https://github.com/neovimhaskell/haskell-vim
""
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskellmode_completion_ghc = 1
let hs_highlight_boolean = 1
let hs_highlight_types = 1
let hs_highlight_more_types = 1
let hs_allow_hash_operator = 1

"" --------------------------------------------------------------------

"" Base 16
"" https://github.com/chriskempson/base16-vim
""
" set t_Co=16
" syntax on
" colorscheme base16-default-dark
" set background=dark

"" vim-one
"" https://github.com/rakr/vim-one
""
" let g:airline_theme='one'
" colorscheme one
" set background=dark " for the dark version
" " set background=light " for the light version

"" https://github.com/rakr/vim-colors-rakr

"" https://github.com/jeffkreeftmeijer/vim-dim
""
" set t_Co=16
" colorscheme dim
" set background=dark

"" https://github.com/kyoz/purify

"" https://github.com/joshdick/onedark.vim

"" https://github.com/andreasvc/vim-256noir
""
" colorscheme 256_noir

"" https://github.com/marcopaganini/termschool-vim-theme
""
" colorscheme termschool

"" vim-solarized8
"" https://github.com/lifepillar/vim-solarized8
""
" let g:solarized_visibility = "normal"
" let g:solarized_diffmode = "normal"
" let g:solarized_italics = 0
" set t_Co=256
" set background=dark
" colorscheme solarized8_high

"" sainnhe/edge
"" https://github.com/sainnhe/edge
""
set background=dark
let g:edge_style = 'neon'
let g:edge_disable_italic_comment = 0
colorscheme edge

"" seiya.vim
"" https://github.com/miyakogi/seiya.vim
""
let g:seiya_auto_enable=1


"" ------------------------------------------------------------------------
"" Colorscheme
"" ------------------------------------------------------------------------

"" Custom
" set t_Co=16
" set t_Co=256
" set background=dark
" syntax on

"" highlight CursorLine term=reverse cterm=reverse gui=reverse
"" highlight CursorLine NONE
"" highlight CursorLine ctermfg=black ctermbg=Cyan gui=reverse

" hi CursorLine   term=bold cterm=bold guibg=white
" hi TabLine      ctermfg=Black  ctermbg=Gray      cterm=NONE
" hi TabLineFill  ctermfg=Black  ctermbg=Gray      cterm=NONE
" hi TabLineSel   ctermfg=Black  ctermbg=Cyan      cterm=NONE
" hi Visual       ctermfg=Black  ctermbg=Gray      cterm=NONE


"" ------------------------------------------------------------------------
"" Syntax
"" ------------------------------------------------------------------------

"" Where to search for swap files.
if has ("unix")
    set directory=.,~/tmp,/tmp,/var/tmp
else
    set directory=.,c:\tmp,c:\temp;C:\WINDOWS\Temp
endif

"" List of directories for the backup file.
if has ("unix")
    set backupdir=.,~/tmp,/tmp,/var/tmp
endif

"" List of file names, separated by commas, that are used to lookup words
"" for keyword completion commands |i_CTRL-X_CTRL-K|
if has ("unix")
    set dictionary=/usr/share/dict/american-english,~/.vim/dict.txt
endif

"" A comma separated list of strings that can start a comment line
"" (1)
set comments=b:\",b:#,:%,fbn:-,fb:*,n:>,n:),:\[---\ snip,:--\
"" (2)
" set comments=sr:/*,mb:*,elx:*/

"" For file name completion you can use the 'suffixes' option to set a
"" priority between files with almost the same name.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

"" You should set the 'errorformat' option to a scanf-like string
"" that describes the format.
set errorformat=%f:%l:\ %m

"" This is a sequence of letters which describes how automatic
"" formatting is to be done.
set formatoptions=tcrqv

"" When non-empty, the viminfo file is read upon startup and written
"" when exiting Vim.
set viminfo='50,\"100,:50

"" Default (for use with linebreak)
set breakat=\ ^I!@*-+_;:,./?

"" Write the contents of the file, if it has been modified.
set autowrite

"" Program to use for the ":make" command.
set makeprg=make\ #<.o

"" Insert two spaces after a '.', '?' and '!' with a join command.
set nojoinspaces

"" Function keys that start with an <Esc> are recognized in Insert mode.
" set esckeys

"" Make a backup before overwriting a file.
set nobackup

"" Don't make a backup before overwriting a file.
set nowritebackup

"" Enable to use of mouse, a->all previous modes.
" set mouse=a
set mouse=""

"" Display all man entries for `K' lookup.
set keywordprg=man\ -a

"" Infer case for ignorecase keyword completion.
set infercase

"" Use PC BIOS to read keyboard, for better ^C detection.
" set bioskey

"" Be incompatible with vi, please.
set nocompatible

"" Do not toggle 'g' and 'c' with ":s".
set noedcompatible

"" Vi compatibility options.
"" (1)
set cpoptions=cFs$
"" (2)
" set cpoptions=aABceFs

"" Binary option.
set nobinary

"" Current and loaded buffers are enough for lookup.
set complete=.,b,u
set completeopt=menuone,menu,longest

"" Allow digraphs with eg o^H/.
" set digraph

"" Only resize current window when splitting.
set noequalalways

"" Always on a linux or unix box (!) :-)
if has ("unix")
    set fileformat=unix
else
    set fileformat=dos
endif

"" Supported fileformats
set fileformats=unix,dos,mac

"" I usually complete variables, functions etc...
set noinfercase

"" Scroll at most 20 lines at a time.
" set ttyscroll=0

"" Indicates a fast terminal connection.
set ttyfast

"" Character you have to type to start wildcard expansion in the command-line.
set wildchar=<TAB>

"" When 'wildmenu' is on, command-line completion operates in an enhanced mode
set wildmenu

"" When on, ":autocmd", shell and write commands are not allowed in .vimrc
"" and ".exrc" in the current directory and map commands are displayed.
set secure

"" Put Vim in Paste mode.  This is useful if you want to cut or copy some text
"" from one window and paste it in Vim.  This will avoid unexpected effects.
"" (1)
" set paste
"" (2)
set nopaste

"" When non-empty, specifies the key sequence that toggles the 'paste' option.
set pastetoggle=<Ins>

"" Pause listings when the whole screen is filled.
set more

"" Precede continued screen lines.
set showbreak=++++

"" Use visual bell instead of beeping.
set visualbell
set t_vb=

"" Ring the bell (beep or screen flash) for error messages.
set noerrorbells

"" Always be in insertmode? That's not vim!
set noinsertmode

"" Split windows below current window.
set splitbelow
set splitright

"" Show matching delimiters.
set showmatch

"" How many tenths of a second to blink matching brackets for
set matchtime=5

"" Number of columns of the screen.
" set columns=80

"" Number of lines of the Vim window.
" set lines=52

"" Backspace all the way to wherever it takes.
"" Allow backspacing over everything in insert mode (except \n & \t)
"" (1)
" set backspace=2
"" (2)
set backspace=indent,start,eol

"" Number of spaces that a <Tab> in the file counts for.
set tabstop=4

"" Number of spaces that a <Tab> counts for while performing editing
"" operations, like inserting a <Tab> or using <BS>.
set softtabstop=4

"" Indent by four columns at a time.
set shiftwidth=4

"" Use multiple of shiftwidth when indenting with '<' and '>'
set shiftround

"" In Insert mode: Use the appropriate number of spaces to insert a <Tab>.
"" don't use tabs
set expandtab

"" Encodings
" let &termencoding = &encoding
"
" set encoding=latin2
" set fileencoding=latin2
" set fileencodings=latin2
"
" set encoding=utf-8
" set fileencoding=utf-8
" set fileencodings=utf-8

"" Maximum width of text that is being inserted.
" set textwidth=78
set textwidth=120

"" At least 5 lines for current window.
set winheight=5

"" 15 lines of help is enough.
set helpheight=15

"" Keep 2 lines above and below cursor.
set scrolloff=3

"" Nowrap lines at the end of screen.
set nowrap

"" I do not display line numbers by default.
"" (1)
set nonumber
"" (2)
" set number

"" Left/right arrow keys wrap.
" set whichwrap=<,>,[,]
set whichwrap=b,s,h,l,<,>,[,]

"" Number of characters from the right window border where wrapping starts.
"" (1)
" set wrapmargin=0
"" (2)
set wrapmargin=1

"" Display unprintable characters
" set nolist
set list
" set listchars=tab:>.,trail:x
set listchars=tab:>.,trail:.,extends:#
"" Do not show <Tab> and <EOL> as ^I and $.

"" Do not redraw while running macros (good performance config)
set lazyredraw

"" Don't insert any extra pixel lines betweens rows
set linespace=0

"" Printing
set printoptions=paper:A4,syntax:y,wrap:y
set printexpr=PrintFile(v:fname_in)
function PrintFile(fname)
    call system("gtklp " . a:fname)
    call delete(a:fname)
    return v:shell_error
endfunc

"" Shell
set shell=/bin/bash

"" Undo
set undolevels=1000
set undoreload=10000

"" Folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

" Highlight current line
set cursorline


"" ------------------------------------------------------------------------
"" Indenting
"" ------------------------------------------------------------------------

"" Copy indent from current line when starting a new line.
"" (1)
" set noautoindent
"" (2)
set autoindent

"" Do smart autoindenting when starting a new line.
set smartindent

" Copy the previous indentation on autoindenting
set copyindent

"" Enables automatic C program indenting.
set cindent

"" C language indent options.
"" (1)
" set cinoptions=e0,n0,f0,{0,}0,^0,c3
"" (2)
set cinoptions=>s,e0,n0,f0,{0,}0,^0,:s,=s,ps,ts,c3,+s,(2s,us,)20,*30,gs,hs

"" String that controls Vim's indenting
"" (1)
" set cinkeys=0{,0},:,0#,!^F,o,O,e
"" (2)
set cinkeys=0{,0},:,0#,!<Tab>,!^F

"" Horizontal scrolling 8 columns at a time.
set sidescroll=8

"" Do not home cursor to beginning of line.
set nostartofline


"" ------------------------------------------------------------------------
"" Look
"" ------------------------------------------------------------------------

"" Enable file type detection
filetype on

"" Enable file type detection.
filetype plugin on
filetype indent on

"" Recognize anything in my .Postponed directory as a news article, and
"" anything at all with a .txt extension as being human-language text
"" [this clobbers the `help' filetype, but that doesn't seem to prevent
"" help from working " properly]:
augroup filetype
    autocmd BufNewFile,BufRead */.Postponed/*,mail set filetype=mail
    autocmd BufNewFile,BufRead *.txt,*.doc,text,README set filetype=human
    autocmd BufNewFile,BufRead *.tex,*.lyx set filetype=tex
    autocmd BufNewFile,BufRead *.t set filetype=perl
    autocmd BufNewFile,BufRead c,cpp,java,slang,perl,python,html,xml,css,sh,php,asp,go,lua set filetype=srcfile
augroup END

"" Expand Tab (use spaces)
autocmd FileType mail set expandtab
autocmd FileType human set expandtab
autocmd FileType tex set expandtab
autocmd FileType haskell set expandtab
autocmd FileType srcfile set expandtab
autocmd FileType make set noexpandtab
autocmd FileType yaml set expandtab

"" For both CSS and HTML, use genuine tab characters for indentation, to
"" make files a few bytes smaller:
autocmd FileType html,xml,css set tabstop=2
autocmd FileType haskell set tabstop=4 softtabstop=4
autocmd FileType yaml set tabstop=2 softtabstop=2

"" ShiftWidth
autocmd FileType mail set shiftwidth=4
autocmd FileType human set shiftwidth=4
autocmd FileType tex set shiftwidth=4
autocmd FileType haskell set shiftwidth=2 shiftround
autocmd FileType srcfile set shiftwidth=4
autocmd FileType make set shiftwidth=4
autocmd FileType yaml set shiftwidth=2

"" TextWitdh
autocmd FileType mail set textwidth=78
autocmd FileType human set textwidth=120
autocmd FileType tex set textwidth=78
autocmd FileType haskell set textwidth=0
autocmd FileType srcfile set textwidth=0
autocmd FileType make set textwidth=0

"" Set number
autocmd FileType mail set nonumber
autocmd FileType human set nonumber
autocmd FileType tex set nonumber
autocmd FileType haskell set number
autocmd FileType srcfile set number
autocmd FileType c,cpp,java,slang,perl,python,html,xml,css,sh,php,asp,go,lua set number
autocmd FileType make set number

"" Set encoding
" autocmd FileType mail set encoding=utf-8 fileencoding=utf-8 termencoding=utf-8 fileencodings=utf-8
" autocmd FileType human set encoding=latin2

if has("multi_byte")
    if &termencoding == ""
        let &termencoding = &encoding
    endif
    autocmd FileType mail set encoding=utf-8
    autocmd FileType human set encoding=utf-8
    set fileencodings=ucs-bom,utf-8,latin2
else
    autocmd FileType mail set encoding=latin2
    autocmd FileType human set encoding=latin2
endif

"" Indenting
autocmd FileType c,cpp,java,slang,go set cindent
autocmd FileType perl,python,css,sh,php,asp,make,lua set autoindent
autocmd FileType mail,human set noautoindent
autocmd FileType tex set cindent
autocmd BufNew,BufEnter,BufAdd,BufCreate * set noautoindent

"" In human-language files, automatically format everything at 78 chars:
autocmd FileType mail,human set formatoptions+=t

"" For actual C (not C++) programming where comments have explicit end
"" characters, if starting a new line in the middle of a comment
"" automatically insert the comment leader characters:
autocmd FileType c set formatoptions+=ro

"" For HTML, generally format text, but if a long line has been created
"" leave it alone when editing:
autocmd FileType html set formatoptions+=tl isk+=:,/,~

"" Check code with :make
autocmd FileType perl set makeprg=perl\ -c\ %\ $*
autocmd FileType perl set errorformat=%f:%l:%m
autocmd FileType python set makeprg=python\ %

" autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"" Automatically chmod +x Shell and Perl scripts
if has ("unix")
    autocmd BufWritePost *.ksh !chmod +x %
    autocmd BufWritePost *.sh !chmod +x %
    autocmd BufWritePost *.pl !chmod +x %
    autocmd BufWritePost *.p6 !chmod +x %
    autocmd BufWritePost *.py !chmod +x %
    autocmd BufWritePost *.rb !chmod +x %
    autocmd BufWritePost *.pm,*.t,*.pl echom system("perl -Ilib -c " . '"' . expand("%:p"). '"' )
endif

"" Ctags
if has ("unix")
    let Tlist_Ctags_Cmd = '/usr/bin/ctags-exuberant'
endif

"" Templates
if has ("unix")
    augroup templates
        au!
        " read in templates files
        autocmd BufNewFile *.* silent! execute '0r ~/.vim/templates/skeleton.'.expand("<afile>:e")
    augroup END
endif


"" ------------------------------------------------------------------------
"" Title
"" ------------------------------------------------------------------------

"" When on the title of the window will be set to "VIM - filename".
" set title

"" Title String
" set titlestring=%<%F%=%l/%L-%P

"" Gives the percentage of 'columns' to use for the length of the window
"" title.
" set titlelen=70


"" ------------------------------------------------------------------------
"" Status line
"" ------------------------------------------------------------------------

"" This option controls various toolbar settings
" set tb=icons

"" Statusline
if &t_Co > 2 || has ("gui_running")
    "" Simple status line (1)
    " set statusline=%<%f\ %h%m%r%=%-10.(%l,%c%V%)[%{&fileformat}]\ %P

    "" Simple status line (2)
    " set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]

    "" Simple status line (3)
    set statusline=[%F%m%r%h%w][%L][%{&ff}]%y[%p%%][%04l,%04v]

    "" New Syntax
    " set statusline=[%n][%1*%<%{getcwd()}%*\]\[%2*%<%F%*\]\[%3*%<%{&ff}%*\]\[%1*%<%M%R%H%W%*\]\ %=\ [%3*%03b,0x%B%*\ %1*%03l/%03L,%03c%*\ %2*%P%*]
    " set statusline=[%n][%1*%<%{getcwd()}%*\]\[%2*%<%F%*\]\[%3*%<%{&ff}%*\]\%y[%1*%<%M%R%H%W%*\]\ %=\ [%3*%03b,0x%B%*\ %1*%03l/%03L,%03c%*\ %2*%P%*]
    " set statusline=[%n%*][%1*%<%{getcwd()}%*\]\[%2*%<%F%*\]\[%3*%<%{&ff}%*\]%1*%y%*\[%1*%<%M%R%H%W%*\]\ %=\ [%3*%03b,0x%B%*\ %1*%03l/%03L,%03c%*\ %2*%P%*]
endif

"" Always show a status line.
set laststatus=2

"" Shorter status messages.
"" (1)
" set shortmess=atI
"" (2)
set shortmess=aOstT

"" Enable ruler on status line.
set ruler
set rulerformat=%15(%c%V\ %p%%%)

"" Show current input mode in status line.
set showmode

"" Show (partial) command in status line.
set showcmd

"" Number of lines to use for the command-line.
set cmdheight=1


"" ------------------------------------------------------------------------
"" Tabs
"" ------------------------------------------------------------------------

"" Maximum number of tab pages to be opened by the |-p| command line argument
"" or the ":tab all" command.
set tabpagemax=15

"" Always show tab page labels
"" (1)
" set showtabline=2
"" (2)
" set showtabline=1
"" (3)
if has ("gui_running")
    set showtabline=2
else
    set showtabline=1
endif


"" ------------------------------------------------------------------------
"" Search
"" ------------------------------------------------------------------------

"" A history of ":" commands, and a history of previous search patterns
"" are remembered.
set history=1024

"" While typing a search pattern, show immediately where the so far typed 
"" pattern matches.
set ignorecase
set smartcase
set smarttab

"" Incremental searching.
set incsearch

"" Searches start over again at end/start of buffer.
set wrapscan

"" Tell me when search wrapped EOF/BOF.
set noterse

"" Special characters can be used in search patterns.
set magic

"" Highlight search pattern
if &t_Co > 1 || has ("gui_running")
    set hlsearch
endif


"" ------------------------------------------------------------------------
"" Key Mappings
"" ------------------------------------------------------------------------

"" Make
if has ("unix")
    map ,rm :!make<CR>
    vmap ,rm <esc>:!make<CR>
    imap ,rm <esc>:!make<CR>

    map ,m :make<cr>
    vmap ,m <esc>:make<cr>
    map ,m <esc>:make<cr>
endif

"" List files in current directory.
if has ("unix")
    map ,ls :!ls<CR>
    vmap ,ls :!ls<CR>
    imap ,ls :!ls<CR>
endif

"" Run LaTeX on the current file
if has ("unix")
    map ,rl :!latex %
endif

"" Spelling
" map <F5> :w<CR>:!ispell %<CR>:e %<CR>
" map #fi :w<CR>:!ispell %<CR>:e %<CR>
" map ^T :w!<CR>:!aspell check %<CR>:e! %<CR>
if has ("unix")
    map ,ri :w<CR>:!ispell -d /home/kmarzic/stuff/spelling/croatian.hash %<CR>:e %<CR>
    map ,ra :w<CR>:!aspell check --lang=hr %<CR>:e %<CR>
endif

"" UTF-8 encoding
if has ("unix")
    map ,rc0 :e ++enc=utf-8<c-m><c-L>
    " Latin-1 encoding
    map ,rc1 :%!lv -Il1 -Ou8<c-m><c-L>
    " Latin-2 encoding
    map ,rc2 :%!lv -Il2 -Ou8<c-m><c-L>
    " UTF-8
    map ,u :set encoding=utf-8 termencoding=utf-8<CR>
endif

"" Tab navigation
"" gt            go to next tab
"" gT            go to previous tab
"" {i}gt         go to tab in position i
map  ,t           <Esc>:tabnew<CR>
nmap <C-n>        :tabnew<CR>
imap <C-n>        <ESC>:tabnew<CR>
nmap <A-PageUp>   :tabprevious<CR>
nmap <A-PageDown> :tabnext<CR>
map  <A-PageUp>   :tabprevious<CR>
map  <A-PageDown> :tabnext<CR>
imap <A-PageUp>   <ESC>:tabprevious<CR>i
imap <A-PageDown> <ESC>:tabnext<CR>i

"" vimrc mapping
nn ,vs :source <C-R>=vimrc<CR><CR>
nn ,ve :edit   <C-R>=vimrc<CR><CR>

"" Delete this whitespaces
map ,vw :%s/\s\+$//<CR><CR>

"" Maximize
map ,vm :set lines=999 columns=999<CR>

"" Change current directory to files directory
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

"" clear empty lines
map ,cel :%s/^\s\+$//

"" delete 'empty' lines
map ,del :g/^\s\+$/d

"" Edit another file in the same directory as the current file uses expression
"" to extract path from current file's path
if has("unix")
    map ,e :e <C-R>=expand("%:p:h") . "/" <CR>
else
    map ,e :e <C-R>=expand("%:p:h") . "\" <CR>
endif

"" Last Update
iab LaUpp Last update: <C-R>=strftime("%Y-%m-%d %H:%M:%S (%Z)")<cr>
iab LaUp <C-R>=strftime("%Y-%m-%d %H:%M:%S (%Z)")<CR>
iab YDATE <C-R>=strftime("%Y-%m-%d %H:%M:%S (%Z)")<CR>
map ,lu 1G/Last update: \s*/e+1<CR>CYDATE<ESC>

"" Current File
iab CuFii Current file: <C-R>=expand("%:t")<CR>
iab CuFi <C-R>=expand("%:t")<CR>
iab CuPaFii Current file: <C-R>=expand("%:p")<CR>
iab CuPaFi <C-R>=expand("%:p")<CR>

"" Custom
iab DiGi 1234567890<Left><Esc>
iab AdLer 123456789_123456789_123456789_123<Left><Esc>
iab RuLer 123456789_123456789_123456789_123456789_123456789_123456789_123456789_12<Left><Esc>
iab CuTe <CR>[...]<Esc>
iab ViM VIM - Vi Improoved, http://www.vim.org<Esc>

"" ------------------------------------------------------------------------
"" eof
"" ------------------------------------------------------------------------
