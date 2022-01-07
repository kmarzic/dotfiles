#!/bin/bash

export PATH=${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin

function __dirs()
{
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

    if [ ! -e ~/.vim/plugged ]
    then
        echo "# mkdir -p ~/.vim/plugged"
        mkdir -p ~/.vim/plugged
    fi

    if [ ! -e ~/.local/share/nvim/site/autoload ]
    then
        echo "# mkdir -p ~/.local/share/nvim/site/autoload/"
        mkdir -p ~/.local/share/nvim/site/autoload
    fi
}

function __neovim_bin()
{
    #### nightly
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage -o ~/bin/nvim
    #### 0.5.0
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.5.0/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.5.0/nvim.appimage -o ~/bin/nvim
    #### 0.5.1
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.5.1/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.5.1/nvim.appimage -o ~/bin/nvim
    #### 0.6.1
    echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.6.1/nvim.appimage -o ~/bin/nvim"
    curl -L https://github.com/neovim/neovim/releases/download/v0.6.1/nvim.appimage -o ~/bin/nvim
    ####
    echo "# chmod +x ~/bin/nvim"
    chmod +x ~/bin/nvim
}

function __neovim_vimplug()
{
    echo "# curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

function __vim_vimplug()
{
    echo "# curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

function __plugin_update()
{
    nvim +PlugInstall +qall
    vim +PlugInstall +qall
}

#### MAIN
__dirs
__neovim_bin
__neovim_vimplug
__vim_vimplug
__plugin_update

###############################################################################
#### END
###############################################################################
