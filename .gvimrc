"" -----------------------------------------------------------------------
"" Author: Kresimir Marzic
"" E-mail: kmarzic@gmail.com
"" Last update: 2017-12-03 20:32:29 (CET)
"" Current file: .gvimrc
"" -----------------------------------------------------------------------


"" -----------------------------------------------------------------------
"" GUI settings
"" -----------------------------------------------------------------------

"" Show the current filename in the title:
"" Many users also want to see the name of the file associated with the current edit buffer in the title:
" let &titlestring = expand ("%:p:~:.:h")
"" (1)
" au BufEnter * let &titlestring = hostname() . "/" . expand("%:p")
"" (2)
au BufEnter * let &titlestring = "VIM [" . expand("%:p") . "]"

"" Normally, Vim takes control of all Alt-<Key> combinations, to increase the
"" number of possible mappings. This clashes with the standard use of Alt as
"" the key for accessing menus.
" simalt ~x

"" To delete a menu item or a whole submenu, use the unmenu commands, which
"" are analogous to the unmap commands.
"" (1)
" :unmenu *
"" (2)
" :unmenu! *

"" This is a list of fonts which will be used for the GUI version of Vim.
if has ("unix")
    "" Fonts
    " set guifont=-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-2
    " set guifont=-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-1
    "
    " set guifont=Courier\ 10\ Pitch\ 9
    " set guifont=Courier\ 10\ Pitch\ 10
    " set guifont=Courier\ 10\ Pitch\ 11
    " set guifont=Courier\ 10\ Pitch\ 12
    "
    " set guifont=Courier\ New\ 9
    " set guifont=Courier\ New\ 10
    " set guifont=Courier\ New\ 11
    " set guifont=Courier\ New\ 12
    "
    " set guifont=Terminus\ 9
    " set guifont=Terminus\ 10
    " set guifont=Terminus\ 11
    " set guifont=Terminus\ 12
    "
    " set guifont=Monospace\ 9
    " set guifont=Monospace\ 10
    " set guifont=Monospace\ 11
    " set guifont=Monospace\ 12
    " set guifont=Monospace\ 13
    "
    set guifont=Inconsolata\ Medium\ 12
else
    " set guifont=Courier:h9:cEASTEUROPE
    set guifont=Courier_New:h11:cANSI
endif

"" Full Screen Mode
" if ! has ("unix")
"     set lines=999 columns=999
" endif

"" WOmdpw Position
" if has ("unix")
"     winsize 180 45
"     winpos 20 20
" endif

"" GUI Options
if has ("unix")
    "" Menu bar
    set guioptions+=m

    "" Toolbar
    set guioptions-=T

    "" Right-hand scrollbar is always present.
    set guioptions+=r

    "" Use console dialogs instead of popup dialogs for simple choices.
    set guioptions+=c
endif

"" UTF Encoding
" if has("multi_byte")
"     set encoding=utf-8
"     set fileencoding=utf-8
"     set fileencodings=utf-8
"     set termencoding=utf-8
" endif

"" Select mode
" set selectmode=mouse,key,cmd

"" GUI Tab
set guitablabel=%N:%M%t " Show tab numbers


"" -----------------------------------------------------------------------
"" Default color scheme
"" -----------------------------------------------------------------------

" colorscheme default

" colorscheme blue
" colorscheme darkblue
" colorscheme default
" colorscheme delek
colorscheme desert ""
" colorscheme elflord
" colorscheme evening
" colorscheme koehler
" colorscheme morning
" colorscheme murphy
" colorscheme pablo
" colorscheme peachpuff
" colorscheme ron
" colorscheme shine
" colorscheme slate
" colorscheme torte
" colorscheme zellner


"" -----------------------------------------------------------------------
"" Custom color scheme
"" -----------------------------------------------------------------------

"" Color Schemes
"" http://vimcolorschemetest.googlecode.com

"" --

"" http://astonj.com/tech/vim-for-ruby-rails-and-a-sexy-theme/
"" Last Change: June 28th, 2012
"" ---> default 2 <---
" colorscheme codeschool

"" http://www.vim.org/scripts/script.php?script_id=2034
"" Last Change: 04 Oct, 2007
" colorscheme colorful256

"" http://www.vim.org/scripts/script.php?script_id=1576
"" ---> default 1 <---
"" Last Change:  $Date: 2006/03/25 09:37:49 $
"" Version:      $Id: crt.vim version 1.0
" colorscheme crt

"" http://vimcolorschemetest.googlecode.com/svn/colors/darkslategray.vim
"" Last Change: 2004 October 05
"" Version: 1.7
" colorscheme darkslategray

"" http://www.vim.org/scripts/script.php?script_id=2650
"" version: 1.1
" colorscheme darkZ

"" http://www.vim.org/scripts/script.php?script_id=2215
"" version: 1.8
"" date: 2009-01-08
" colorscheme darkspectrum

"" http://www.vim.org/scripts/script.php?script_id=1802
" Last Change: 2007-03-19
" Version: 1.0.2
" colorscheme eclipse

"" http://www.vim.org/scripts/script.php?script_id=1253
"" Version: 2016.12.07
" colorscheme greens

"" http://www.vim.org/scripts/script.php?script_id=2155
"" Last Change: 6.6.2008
" colorscheme greenvision

"" http://www.vim.org/scripts/script.php?script_id=1483
"" Last Modified: Nov, 30 2006 (13:01)
"" Version:       0.5
" colorscheme marklar

"" http://www.vim.org/scripts/script.php?script_id=1354
"" Version: 1.0
" colorscheme night_vision

"" http://www.vim.org/scripts/script.php?script_id=2577
"" Last Change: 2009-03-09
" colorscheme summerfruit256

"" http://www.vim.org/scripts/script.php?script_id=1718
"" Last Modified: 2010-04-05
"" Version:       1.4.2
"" ---> default 2 <---
" colorscheme tabula

"" http://www.vim.org/scripts/script.php?script_id=791
"" Version: 0.6.4
" colorscheme relaxedgreen

"" http://files.werx.dk/wombat.vim
"" Last Change: January 22 2007
" colorscheme wombat

"" http://www.vim.org/scripts/script.php?script_id=2140
"" Version: 1.6
" colorscheme xoria256

"" http://slinky.imukuppi.org/zenburnpage/
"" Last Change:  $Id: zenburn.vim,v 2.21 2011/04/26 12:13:41 slinky Exp slinky $
" colorscheme zenburn

"" --

"" Color Scheme - etkkrma
" colorscheme etkkrma
" colorscheme night_vision_etkkrma
" colorscheme green_black_etkkrma


"" -----------------------------------------------------------------------
"" Custom color scheme (in bundle)
"" -----------------------------------------------------------------------

"" last256 (v1.1.0)
"" http://www.vim.org/scripts/script.php?script_id=4489
"" https://github.com/sk1418/last256
"" cd ~/.vim/bundle; git clone https://github.com/sk1418/last256
"" cd ~/.vim/bundle/last256; git remote update
"" NOTE: configured in ~/.gvimrc
" colorscheme last256

"" LithoChromatic Color Theme (1.2)
"" http://www.vim.org/scripts/script.php?script_id=4406
"" https://github.com/vim-scripts/lithochromatic
"" cd ~/.vim/bundle; git clone https://github.com/vim-scripts/lithochromatic
"" cd ~/.vim/bundle/lithochromatic; git remote update
"" NOTE: configured in ~/.gvimrc
" colorscheme lithochromatic

"" Solarized Colorscheme for Vim (2011-Apr-16)
"" http://ethanschoonover.com/solarized
"" http://www.vim.org/scripts/script.php?script_id=3520
"" https://github.com/altercation/vim-colors-solarized
"" cd ~/.vim/bundle; git clone https://github.com/altercation/vim-colors-solarized.git
"" cd ~/.vim/bundle/vim-colors-solarized; git remote update
"" NOTE: configured in ~/.gvimrc
" let g:solarized_termcolors=256
" set background=light
" set background=dark
" colorscheme solarized

"" vim-hemisu (3.4)
"" http://www.vim.org/scripts/script.php?script_id=4470
"" https://github.com/noahfrederick/vim-hemisu
"" cd ~/.vim/bundle; git clone https://github.com/noahfrederick/vim-hemisu
"" cd ~/.vim/bundle/vim-hemisu; git remote update
"" NOTE: configured in ~/.gvimrc
" set background=light
" set background=dark
" colorscheme hemisu

"" -----------------------------------------------------------------------
"" EOF
"" -----------------------------------------------------------------------
