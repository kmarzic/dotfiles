#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin

## cat ~/.vimrc | grep "git remote update" | awk -F 'cd' '{ print "cd" $2 }'

if [ ! -e ~/.vim ]
then
    echo "# mkdir -p ~/.vim"
    mkdir -p ~/.vim
fi

if [ ! -e ~/.vim/bundle ]
then
    echo "# mkdir -p ~/.vim/bundle"
    mkdir -p ~/.vim/bundle
fi

if [ ! -e ~/.vim/autoload ]
then
    echo "# mkdir -p ~/.vim/autoload"
    mkdir -p ~/.vim/autoload
fi

#### Pathogen

echo "# curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim"
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim

#### Plugins

if [ -e ~/.vim/bundle/Vundle.vim ]
then
    echo "# cd ~/.vim/bundle/Vundle.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/Vundle.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/gmarik/Vundle.vim.git"
    cd ~/.vim/bundle; git clone https://github.com/gmarik/Vundle.vim.git
fi

if [ -e ~/.vim/bundle/bufexplorer.zip ]
then
    echo "# cd ~/.vim/bundle/bufexplorer.zip; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/bufexplorer.zip; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/bufexplorer.zip"
    cd ~/.vim/bundle; git clone https://github.com/vim-scripts/bufexplorer.zip
fi

# if [ -e ~/.vim/bundle/Command-T ]
# then
#     echo "# cd ~/.vim/bundle/Command-T; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/Command-T; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/wincent/Command-T"
#     cd ~/.vim/bundle; git clone https://github.com/wincent/Command-T
# fi

if [ -e ~/.vim/bundle/ctrlp.vim ]
then
    echo "# cd ~/.vim/bundle/ctrlp.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/ctrlp.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/kien/ctrlp.vim"
    cd ~/.vim/bundle; git clone https://github.com/kien/ctrlp.vim
fi

if [ -e ~/.vim/bundle/nerdtree ]
then
    echo "# cd ~/.vim/bundle/nerdtree; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/nerdtree; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/scrooloose/nerdtree.git"
    cd ~/.vim/bundle; git clone https://github.com/scrooloose/nerdtree.git
fi

if [ -e ~/.vim/bundle/supertab ]
then
    echo "# cd ~/.vim/bundle/supertab; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/supertab; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/ervandew/supertab"
    cd ~/.vim/bundle; git clone https://github.com/ervandew/supertab
fi

# if [ -e ~/.vim/bundle/YouCompleteMe ]
# then
#     echo "# cd ~/.vim/bundle/YouCompleteMe; git remote update; git submodule update --recursive "
#     cd ~/.vim/bundle/YouCompleteMe; git remote update; git submodule update --recursive
# 
#     echo "# cd ~/.vim/bundle/YouCompleteMe; ./install.py --all"
#     cd ~/.vim/bundle/YouCompleteMe; ./install.py --all
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/Valloric/YouCompleteMe"
#     cd ~/.vim/bundle; git clone https://github.com/Valloric/YouCompleteMe
# 
#     echo "# cd ~/.vim/bundle/YouCompleteMe; git remote update; git submodule update --recursive "
#     cd ~/.vim/bundle/YouCompleteMe; git remote update; git submodule update --recursive
# fi

if [ -e ~/.vim/bundle/syntastic ]
then
    echo "# cd ~/.vim/bundle/syntastic; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/syntastic; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/scrooloose/syntastic.git"
    cd ~/.vim/bundle; git clone https://github.com/scrooloose/syntastic.git
fi

if [ -e ~/.vim/bundle/tabline.vim ]
then
    echo "# cd ~/.vim/bundle/tabline.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/tabline.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/mkitt/tabline.vim.git"
    cd ~/.vim/bundle; git clone https://github.com/mkitt/tabline.vim.git
fi

if [ -e ~/.vim/bundle/tagbar ]
then
    echo "# cd ~/.vim/bundle/tagbar; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/tagbar; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/majutsushi/tagbar"
    cd ~/.vim/bundle; git clone https://github.com/majutsushi/tagbar
fi

# if [ -e ~/.vim/bundle/taglist.vim ]
# then
#     echo "# cd ~/.vim/bundle/taglist.vim; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/taglist.vim; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/taglist.vim"
#     cd ~/.vim/bundle; git clone https://github.com/vim-scripts/taglist.vim
# fi

#### Plugins - Languages

if [ -e ~/.vim/bundle/bash-support.vim ]
then
    echo "# cd ~/.vim/bundle/bash-support.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/bash-support.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/bash-support.vim"
    cd ~/.vim/bundle; git clone https://github.com/vim-scripts/bash-support.vim
fi

if [ -e ~/.vim/bundle/c.vim ]
then
    echo "# cd ~/.vim/bundle/c.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/c.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/c.vim"
    cd ~/.vim/bundle; git clone https://github.com/vim-scripts/c.vim
fi

if [ -e ~/.vim/bundle/go.vim ]
then
    echo "# cd ~/.vim/bundle/go.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/go.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/fsouza/go.vim"
    cd ~/.vim/bundle; git clone https://github.com/fsouza/go.vim
fi

if [ -e ~/.vim/bundle/perl-support.vim ]
then
    echo "# cd ~/.vim/bundle/perl-support.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/perl-support.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/perl-support.vim"
    cd ~/.vim/bundle; git clone https://github.com/vim-scripts/perl-support.vim
fi

if [ -e ~/.vim/bundle/vim-perl ]
then
    echo "# cd ~/.vim/bundle/vim-perl; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/vim-perl; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/vim-perl/vim-perl"
    cd ~/.vim/bundle; git clone https://github.com/vim-perl/vim-perl
fi

if [ -e ~/.vim/bundle/python-syntax ]
then
    echo "# cd ~/.vim/bundle/python-syntax; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/python-syntax; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/hdima/python-syntax"
    cd ~/.vim/bundle; git clone https://github.com/hdima/python-syntax
fi

# if [ -e ~/.vim/bundle/jedi-vim ]
# then
#     echo "# cd ~/.vim/bundle/jedi-vim; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/jedi-vim; git remote update; git pull --no-rebase
#
#     echo "# cd ~/.vim/bundle/jedi-vim; git submodule update --init"
#     cd ~/.vim/bundle/jedi-vim; git submodule update --init
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/davidhalter/jedi-vim"
#     cd ~/.vim/bundle; git clone https://github.com/davidhalter/jedi-vim
#
#     echo "# cd ~/.vim/bundle/jedi-vim; git submodule update --init"
#     cd ~/.vim/bundle/jedi-vim; git submodule update --init
# fi

if [ -e ~/.vim/bundle/haskell-vim ]
then
    echo "# cd ~/.vim/bundle/haskell-vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/haskell-vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/neovimhaskell/haskell-vim.git"
    cd ~/.vim/bundle; git clone https://github.com/neovimhaskell/haskell-vim.git
fi

#### Plugins - Color scheme

# if [ -e ~/.vim/bundle/last256 ]
# then
#     echo "# cd ~/.vim/bundle/last256; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/last256; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/sk1418/last256"
#     cd ~/.vim/bundle; git clone https://github.com/sk1418/last256
# fi

# if [ -e ~/.vim/bundle/lithochromatic ]
# then
#     echo "# cd ~/.vim/bundle/lithochromatic; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/lithochromatic; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/vim-scripts/lithochromatic"
#     cd ~/.vim/bundle; git clone https://github.com/vim-scripts/lithochromatic
# fi

# if [ -e ~/.vim/bundle/vim-colors-solarized ]
# then
#     echo "# cd ~/.vim/bundle/vim-colors-solarized; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/vim-colors-solarized; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/altercation/vim-colors-solarized.git"
#     cd ~/.vim/bundle; git clone https://github.com/altercation/vim-colors-solarized.git
# fi

if [ -e ~/.vim/bundle/vim-colors-solarized-black ]
then
    echo "# cd ~/.vim/bundle/vim-colors-solarized-black; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/vim-colors-solarized-black; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/dolph/vim-colors-solarized-black.git"
    cd ~/.vim/bundle; git clone https://github.com/dolph/vim-colors-solarized-black.git
fi

if [ -e ~/.vim/bundle/base16-vim ]
then
    echo "# cd ~/.vim/bundle/base16-vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/base16-vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/chriskempson/base16-vim.git"
    cd ~/.vim/bundle; git clone https://github.com/chriskempson/base16-vim.git
fi

if [ -e ~/.vim/bundle/flattened ]
then
    echo "# cd ~/.vim/bundle/flattened; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/flattened; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/romainl/flattened"
    cd ~/.vim/bundle; git clone https://github.com/romainl/flattened
fi

# if [ -e ~/.vim/bundle/vim-hemisu ]
# then
#     echo "# cd ~/.vim/bundle/vim-hemisu; git remote update; git pull --no-rebase"
#     cd ~/.vim/bundle/vim-hemisu; git remote update; git pull --no-rebase
# else
#     echo "# cd ~/.vim/bundle; git clone https://github.com/noahfrederick/vim-hemisu"
#     cd ~/.vim/bundle; git clone https://github.com/noahfrederick/vim-hemisu
# fi

if [ -e ~/.vim/bundle/vim-solarized8 ]
then
    echo "# cd ~/.vim/bundle/vim-solarized8; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/vim-solarized8; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/lifepillar/vim-solarized8"
    cd ~/.vim/bundle; git clone https://github.com/lifepillar/vim-solarized8
fi

if [ -e ~/.vim/bundle/seiya.vim ]
then
    echo "# cd ~/.vim/bundle/seiya.vim; git remote update; git pull --no-rebase"
    cd ~/.vim/bundle/seiya.vim; git remote update; git pull --no-rebase
else
    echo "# cd ~/.vim/bundle; git clone https://github.com/miyakogi/seiya.vim"
    cd ~/.vim/bundle; git clone https://github.com/miyakogi/seiya.vim
fi

#### END
