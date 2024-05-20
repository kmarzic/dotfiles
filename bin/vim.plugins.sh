#!/bin/bash

#### PATH
export __PATH_ORIG="${PATH}"
#
export __PATH_NEW="${HOME}/bin:${HOME}/.local/bin:/usr/local/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/perl5/bin:/opt/perl5/cpan/bin:${HOME}/perl5/bin"
# export __PATH_NEW="${__PATH_NEW}:/opt/cperl5/bin:/opt/cperl5/cpan/bin"
# export __PATH_NEW="${__PATH_NEW}:/opt/rakudo-star-2017.10/bin:/opt/rakudo-star-2017.10/share/perl6/site/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/python3/bin:/opt/python3.ext/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/emacs/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/lua/bin:/opt/luarocks/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/tmux/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/rust/bin"
export __PATH_NEW="${__PATH_NEW}:/opt/nvim/usr/bin"
export __PATH_NEW="${__PATH_NEW}:${ORACLE_INSTANT_CLIENT}"
export __PATH_NEW="${__PATH_NEW}:${GOROOT}/bin"
export __PATH_NEW="${__PATH_NEW}:${GHCROOT}/bin"
# export __PATH_NEW="${__PATH_NEW}:${HOME}/.cabal/bin"
#
export PATH="${__PATH_NEW}:${__PATH_ORIG}"
# PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

function __dirs_vim()
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
}

function __dirs_nvim()
{
    if [ ! -e ~/.config/nvim ]
    then
        echo "# mkdir -p ~/.config/nvim"
        mkdir -p ~/.config/nvim
    fi

    if [ ! -e ~/.config/nvim/autoload ]
    then
        echo "# mkdir -p ~/.config/nvim/autoload"
        mkdir -p ~/.config/nvim/autload
    fi

    if [ ! -e ~/.local/share/nvim/site/plugged ]
    then
        echo "# mkdir -p ~/.local/share/nvim/site/plugged"
        mkdir -p ~/.local/share/nvim/site/plugged
    fi

    if [ ! -e ~/.local/share/nvim/site/autoload ]
    then
        echo "# mkdir -p ~/.local/share/nvim/site/autoload"
        mkdir -p ~/.local/share/nvim/site/autoload
    fi
}

function __neovim_bin()
{
    echo .

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
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.6.1/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.6.1/nvim.appimage -o ~/bin/nvim
    #### 0.7.2
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.7.2/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.7.2/nvim.appimage -o ~/bin/nvim
    #### 0.8.0
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.8.0/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.8.0/nvim.appimage -o ~/bin/nvim
    #### 0.8.1
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.8.1/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.8.1/nvim.appimage -o ~/bin/nvim
    #### 0.8.2
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.8.2/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.8.2/nvim.appimage -o ~/bin/nvim
    #### 0.8.3
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.8.3/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.8.3/nvim.appimage -o ~/bin/nvim
    #### 0.9.0
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.9.0/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.9.0/nvim.appimage -o ~/bin/nvim
    #### 0.9.1
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.9.1/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.9.1/nvim.appimage -o ~/bin/nvim
    #### 0.9.4
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.9.4/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.9.4/nvim.appimage -o ~/bin/nvim
    #### 0.9.5
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.9.5/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.9.5/nvim.appimage -o ~/bin/nvim
    #### 0.10.0
    # echo "# curl -L https://github.com/neovim/neovim/releases/download/v0.10.0/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim/releases/download/v0.10.0/nvim.appimage -o ~/bin/nvim
    # echo "# curl -L https://github.com/neovim/neovim-releases/releases/download/v0.10.0/nvim.appimage -o ~/bin/nvim"
    # curl -L https://github.com/neovim/neovim-releases/releases/download/v0.10.0/nvim.appimage -o ~/bin/nvim
    ####
    # echo "# chmod +x ~/bin/nvim"
    # chmod +x ~/bin/nvim
}

function __vim_vimplug()
{
    echo "# curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

function __neovim_vimplug()
{
    echo "# curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

function __plugin_update()
{
    ## DEBUG
    # vim +PlugInstall
    # nvim +PlugInstall

    ## NO DEBUG
    vim +PlugInstall +qall
    nvim +PlugInstall +qall
}

#### MAIN
__dirs_vim
__dirs_nvim
__neovim_bin
__vim_vimplug
__neovim_vimplug
__plugin_update

###############################################################################
#### END
###############################################################################
