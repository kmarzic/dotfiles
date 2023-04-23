#!/bin/bash
#===============================================================================
#
#          FILE: screen.toggle.sh
#
#         USAGE: ./screen.toggle.sh [ -h | -x | -s <theme> ]
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2023-03-30
#      REVISION: ---
#===============================================================================

export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### Banner
BANNER="Screen Toggle"

#### Debug
DEBUG=0 # debug is off
# DEBUG=1 # debug is on

## Exit
EXIT_OK=0
EXIT_ERROR=1


###############################################################################
## Functions
###############################################################################

## Function: Printf
##
function __printf()
{
    ## defining colors for outputs
    RED='\033[31m'
    GREEN='\033[32m'
    YELLOW='\033[33m'
    BOLD='\033[1m'
    NORMAL='\033[m'

    [ "${3-}" = "nolb" ] && ECHOSWITCH="-ne" || ECHOSWITCH="-e"

    if [[ ! -z ${2-} && ! -z ${1-} ]]
    then
        case ${2} in
            error)
                echo -e "${RED}${1}${NORMAL}" >&2
                ;;
            info)
                echo ${ECHOSWITCH} "${YELLOW}${1}${NORMAL}"
                ;;
            success)
                echo -e "${GREEN}${1}${NORMAL}"
                ;;
            header)
                echo -e "${BOLD}${1}${NORMAL}"
                ;;
            debug)
                [ ${DEBUG-} -eq 1 ] && echo -e "${1}"
                ;;
            log)
                if [ ${LOG_ENABLED-} ]
                then
                    if [ ! -d ${LOG_DIR} ]
                    then
                        mkdir ${LOG_DIR}
                    fi

                    echo -e "$(date +%Y%m%dT%H%M%S);${1}" >> ${LOG_FILE-}
                fi
                ;;
            *)
                echo -e "${1}"
                ;;
        esac
    else
        echo "${1}"
    fi
}


## Function: Banner
##
function __banner()
{
    __printf "${BANNER}" info
}


#### Function: Help
####
__help()
{
    __printf "Usage: ${0} [ -h | -x | -s <theme> ]"
    __printf "  -h                Help"
    __printf "  -x                Xrandr"
    __printf "  -s <theme>        Theme"
    __printf "  -l <file>  Log to <file>"
    __printf "Examples:"
    __printf "   ${0} -x"
    __printf "   ${0} -s ansi"
    __printf "   ${0} -s base16-atelier-lakeside-light"
    __printf "   ${0} -s base16-google-light"
    __printf "   ${0} -s base16-gruvbox-dark-soft"
    __printf "   ${0} -s catppuccin"
    __printf "   ${0} -s catppuccin.latte"
    __printf "   ${0} -s doom-one"
    __printf "   ${0} -s dracula"
    __printf "   ${0} -s everforest"
    __printf "   ${0} -s lucario"
    __printf "   ${0} -s gruvbox.dark"
    __printf "   ${0} -s gruvbox.light"
    __printf "   ${0} -s monokai"
    __printf "   ${0} -s nord"
    __printf "   ${0} -s papercolor.light"
    __printf "   ${0} -s selenized.dark"
    __printf "   ${0} -s selenized.light"
    __printf "   ${0} -s selenized.white"
    __printf "   ${0} -s solarized.dark"
    __printf "   ${0} -s solarized.light"
    __printf "   ${0} -s srcery"
    __printf "   ${0} -s tokyo.night"
}


## Function: Theme
##
function __theme()
{
    __printf "Theme" info

    #### arg
    theme=${1}
    __printf "theme='${theme}'" debug

    #### set theme
    case ${theme} in
        "ansi")
            __printf "ansi"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s gray/minimalistic-gray-2560x1440-wallpaper-2109511.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/170583-gorgerous-light-gray-background-2000x2000.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/gXswE4.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gray/kNse8Ue.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/Minimalistic_gray_colors_2560x1600.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.ansi ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.ansi .Xdefaults
            [[ -e ~/.Xdefaults.ansi ]] && xrdb -load ~/.Xdefaults.ansi

            #### vim
            [[ -e ~/.vimrc.ansi ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.ansi .vimrc
            [[ -e ~/.gvimrc.ansi ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.ansi .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.ansi.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.ansi.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.ansi ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.ansi dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.ansi.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.ansi.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.ansi ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.ansi .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.ansi ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.ansi .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.ansi.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.ansi.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.ansi.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.ansi.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.ansi.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.ansi.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.ansi.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.ansi.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "base16-atelier-lakeside-light")
            __printf "base16-atelier-lakeside-light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.jpg && ln -s gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.base16-atelier-lakeside-light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.base16-atelier-lakeside-light .Xdefaults
            [[ -e ~/.Xdefaults.base16-atelier-lakeside-light ]] && xrdb -load ~/.Xdefaults.base16-atelier-lakeside-light

            #### vim
            [[ -e ~/.vimrc.base16-atelier-lakeside-light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.base16-atelier-lakeside-light .vimrc
            [[ -e ~/.gvimrc.base16-atelier-lakeside-light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.base16-atelier-lakeside-light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.base16-atelier-lakeside-light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.base16-atelier-lakeside-light.vim init.vim && cd -

            #### redshift.conf
            [[ -e ~/.config/redshift.base16-atelier-lakeside-light.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.base16-atelier-lakeside-light.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.base16-atelier-lakeside-light ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.base16-atelier-lakeside-light .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.base16-atelier-lakeside-light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.base16-atelier-lakeside-light .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.base16-atelier-lakeside-light.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.base16-atelier-lakeside-light.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.base16-atelier-lakeside-light.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.base16-atelier-lakeside-light.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.base16-atelier-lakeside-light.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.base16-atelier-lakeside-light.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.base16-atelier-lakeside-light.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.base16-atelier-lakeside-light.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "base16-google-light")
            __printf "base16-google-light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.jpg && ln -s gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg bg.jpg && cd -
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.base16-google-light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.base16-google-light .Xdefaults
            [[ -e ~/.Xdefaults.base16-google-light ]] && xrdb -load ~/.Xdefaults.base16-google-light

            #### vim
            [[ -e ~/.vimrc.base16-google-light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.base16-google-light .vimrc
            [[ -e ~/.gvimrc.base16-google-light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.base16-google-light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.base16-google-light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.base16-google-light.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.base16-google-light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.base16-google-light .tmux.conf

            #### redshift.conf
            [[ -e ~/.config/redshift.base16-google-light.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.base16-google--light.conf redshift.conf && cd -
            ;;
        "base16-gruvbox-dark-soft")
            __printf "base16-gruvbox-dark-soft"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.base16-gruvbox-dark-soft ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.base16-gruvbox-dark-soft .Xdefaults
            [[ -e ~/.Xdefaults.base16-gruvbox-dark-soft ]] && xrdb -load ~/.Xdefaults.base16-gruvbox-dark-soft

            #### vim
            [[ -e ~/.vimrc.base16-gruvbox-dark-soft ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.base16-gruvbox-dark-soft .vimrc
            [[ -e ~/.gvimrc.base16-gruvbox-dark-soft ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.base16-gruvbox-dark-soft .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.base16-gruvbox-dark-soft.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.base16-gruvbox-dark-soft.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.gruvbox ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.gruvbox dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.base16-gruvbox-dark-soft.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.base16-gruvbox-dark-soft.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.base16-gruvbox-dark-soft ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.base16-gruvbox-dark-soft .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.base16-gruvbox-dark-soft ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.base16-gruvbox-dark-soft .tmux.conf
            ;;
        "catppuccin")
            __printf "catppuccin"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.catppuccin ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.catppuccin .Xdefaults
            [[ -e ~/.Xdefaults.catppuccin ]] && xrdb -load ~/.Xdefaults.catppuccin

            #### vim
            [[ -e ~/.vimrc.catppuccin ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.catppuccin .vimrc
            [[ -e ~/.gvimrc.catppuccin ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.catppuccin .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.catppuccin.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.catppuccin.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.catppuccin ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.catppuccin .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.catppuccin.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.catppuccin.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.catppuccin.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.catppuccin.yml scratchpad.yml && cd -
            ;;
        "catppuccin.latte")
            __printf "catppuccin.latte"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.jpg && ln -s gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.catppuccin.latte ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.catppuccin.latte .Xdefaults
            [[ -e ~/.Xdefaults.catppuccin.latte ]] && xrdb -load ~/.Xdefaults.catppuccin.latte

            #### vim
            [[ -e ~/.vimrc.catppuccin.latte ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.catppuccin.latte .vimrc
            [[ -e ~/.gvimrc.catppuccin.latte ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.catppuccin.latte .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.catppuccin.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.catppuccin.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.catppuccin.latte ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.catppuccin.latte .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.catppuccin.latte.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.catppuccin.latte.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.catppuccin.latte.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.catppuccin.latte.yml scratchpad.yml && cd -
            ;;
        "doom-one")
            __printf "doom-one"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.doom-one ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.doom-one .Xdefaults
            [[ -e ~/.Xdefaults.doom-one ]] && xrdb -load ~/.Xdefaults.doom-one

            #### vim
            [[ -e ~/.vimrc.doom-one ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.doom-one .vimrc
            [[ -e ~/.gvimrc.doom-one ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.doom-one .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.doom-one.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.doom-one.vim init.vim && cd -

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.doom-one.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.doom-one.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.doom-one.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.doom-one.yml scratchpad.yml && cd -
            ;;
        "dracula")
            __printf "dracula"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/batman-nebula-4k-z4.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/outer-digital-space-tq.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/1314200.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/31367.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/655045.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s space/729708.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.png && ln -s dracula/base.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s dracula/dracula.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s dracula/dracula-purplish.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s dracula/Ycjig3q.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.dracula ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.dracula .Xdefaults
            [[ -e ~/.Xdefaults.dracula ]] && xrdb -load ~/.Xdefaults.dracula

            #### vim
            [[ -e ~/.vimrc.dracula ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.dracula .vimrc
            [[ -e ~/.gvimrc.dracula ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.dracula .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.dracula.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.dracula.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.dracula ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.dracula dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.dracula.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.dracula.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.dracula ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.dracula .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.dracula ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.dracula .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.dracula.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.dracula.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.dracula.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.dracula.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.dracula.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.dracula.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.dracula.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.dracula.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "everforest")
            __printf "everforest"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s green/01568_greenparadise_1920x1080.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s green/390_8937_4.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.everforest ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.everforest .Xdefaults
            [[ -e ~/.Xdefaults.everforest ]] && xrdb -load ~/.Xdefaults.everforest

            #### vim
            [[ -e ~/.vimrc.everforest ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.everforest .vimrc
            [[ -e ~/.gvimrc.everforest ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.everforest .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.everforest.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.everforest.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.everforest ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.everforest .tmux.conf
            ;;
        "gruvbox.dark")
            __printf "gruvbox.dark"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.gruvbox.dark ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.gruvbox.dark .Xdefaults
            [[ -e ~/.Xdefaults.gruvbox.dark ]] && xrdb -load ~/.Xdefaults.gruvbox.dark

            #### vim
            [[ -e ~/.vimrc.gruvbox.dark ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.gruvbox.dark .vimrc
            [[ -e ~/.gvimrc.gruvbox.dark ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.gruvbox.dark .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.gruvbox.dark.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.gruvbox.dark.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.gruvbox.dark ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.gruvbox.dark dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.gruvbox.dark.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.gruvbox.dark.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.gruvbox.dark ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.gruvbox.dark .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.gruvbox.dark ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.gruvbox.dark .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.gruvbox.dark.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.gruvbox.dark.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.gruvbox.dark.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.gruvbox.dark.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.gruvbox.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.gruvbox.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.gruvbox.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.gruvbox.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "gruvbox.light")
            __printf "gruvbox.light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.gruvbox.light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.gruvbox.light .Xdefaults
            [[ -e ~/.Xdefaults.gruvbox.light ]] && xrdb -load ~/.Xdefaults.gruvbox.light

            #### vim
            [[ -e ~/.vimrc.gruvbox.light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.gruvbox.light .vimrc
            [[ -e ~/.gvimrc.gruvbox.light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.gruvbox.light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.gruvbox.light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.gruvbox.light.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.gruvbox.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.gruvbox.light .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.gruvbox.light.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.gruvbox.light.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.gruvbox.light.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.gruvbox.light.yml scratchpad.yml && cd -
            ;;
        "lucario")
            __printf "lucario"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.lucario ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.lucario .Xdefaults
            [[ -e ~/.Xdefaults.lucario ]] && xrdb -load ~/.Xdefaults.lucario

            #### vim
            [[ -e ~/.vimrc.lucario ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.lucario .vimrc
            [[ -e ~/.gvimrc.lucario ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.lucario .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.lucario.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.lucario.vim init.vim && cd -

            #### tmux.conf
            [[ -e ~/.tmux.conf.lucario ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.lucario .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.lucario.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.lucario.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.lucario.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.lucario.yml scratchpad.yml && cd -
            ;;
        "monokai")
            __printf "monokai"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.monokai ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.monokai .Xdefaults
            [[ -e ~/.Xdefaults.monokai ]] && xrdb -load ~/.Xdefaults.monokai

            #### vim
            [[ -e ~/.vimrc.monokai ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.monokai .vimrc
            [[ -e ~/.gvimrc.monokai ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.monokai .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.monokai.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.monokai.vim init.vim && cd -

            #### redshift.conf
            [[ -e ~/.config/redshift.monokai.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.monokai.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.monokai ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.monokai .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.monokai ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.monokai .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.monokai.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.monokai.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.monokai.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.monoki.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.monokai.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.monokai.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.monokai.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.monokai.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "nord")
            __printf "nord"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/1586853699_nord-night.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/1586853721_debora-pilati-dog0z4-gqp0-unsplash-modded.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/1586853730_nord-peeks.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/1586853757_amir-matin-pour-unsplash-modded.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/1586853777_nord-day.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nord/2560x1080.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nord/1586853722_kurt-cotoaga-cqblg3lzepk-unsplash-modded.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nord/1586853756_damian-mccoig-2hqpqsqy0zg-unsplash-modded.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nord/1586853769_fabrizio-conti-mbm0wnj5emc-unsplash-modded.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nord/1586853771_daniel-leone-v7datklzzaw-unsplash-modded.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nord/1586853772_gabriel-santiago-lnetu9zcwpm-unsplash-modded.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.nord ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.nord .Xdefaults
            [[ -e ~/.Xdefaults.nord ]] && xrdb -load ~/.Xdefaults.nord

            #### vim
            [[ -e ~/.vimrc.nord ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.nord .vimrc
            [[ -e ~/.gvimrc.nord ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.nord .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.nord.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.nord.vim init.vim && cd -

            #### redshift.conf
            [[ -e ~/.config/redshift.nord.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.nord.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.nord ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.nord .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.nord ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.nord .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.nord.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.nord.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.nord.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.nord.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.nord.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.nord.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.nord.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.nord.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi
            ;;
        "papercolor.light")
            __printf "papercolor light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.jpg && ln -s gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.papercolor.light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.papercolor.light .Xdefaults
            [[ -e ~/.Xdefaults.papercolor.light ]] && xrdb -load ~/.Xdefaults.papercolor.light

            #### vim
            [[ -e ~/.vimrc.papercolor.light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.papercolor.light .vimrc
            [[ -e ~/.gvimrc.papercolor.light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.papercolor.light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.papercolor.light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.papercolor.light.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.papercolor.light ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.papercolor.light dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.papercolor.light.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.papercolor.light.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.papercolor.light ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.papercolor.light .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.papercolor.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.papercolor.light .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.papercolor.light.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.papercolor.light.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.papercolor.light.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.papercolor.light.yml scratchpad.yml && cd -
            ;;
        "selenized.dark")
            __printf "selenized dark"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/seed_of_life_by_lekremyelsew-d7bfnwj.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/dVMZsMn.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/BaocXcW.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/1OlbRnT.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/6aEOSOS.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/hnSuKgw.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/burst2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.selenized.dark ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.selenized.dark .Xdefaults
            [[ -e ~/.Xdefaults.selenized.dark ]] && xrdb -load ~/.Xdefaults.selenized.dark

            #### vim
            [[ -e ~/.vimrc.selenized.dark ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.selenized.dark .vimrc
            [[ -e ~/.gvimrc.selenized.dark ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.selenized.dark .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.selenized.dark.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.selenized.dark.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.selenized.dark ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.selenized.dark dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.selenized.dark.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.selenized.dark.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.selenized.dark ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.selenized.dark .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.selenized.dark ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.selenized.dark .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.selenized.dark.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.selenized.dark.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.selenized.dark.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.selenized.dark.yml scratchpad.yml && cd -
            ;;
        "selenized.light")
            __printf "selenized light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/AB_Wallpaper_Light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/burst1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized-mountains-light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/d48d4ca9f67739f39d2199e30ee3ec68c24e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarizedlightstripes.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/wallpaperbetter_light.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/Screenshot_20220519_131646.png bg.png && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.selenized.light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.selenized.light .Xdefaults
            [[ -e ~/.Xdefaults.selenized.light ]] && xrdb -load ~/.Xdefaults.selenized.light

            #### vim
            [[ -e ~/.vimrc.selenized.light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.selenized.light .vimrc
            [[ -e ~/.gvimrc.selenized.light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.selenized.light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.selenized.light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.selenized.light.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.selenized.light ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.selenized.light dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.selenized.light.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.selenized.light.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.selenized.light ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.selenized.light .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.selenized.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.selenized.light .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.selenized.light.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.selenized.light.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.selenized.light.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.selenized.light.yml scratchpad.yml && cd -
            ;;
        "selenized.white")
            __printf "selenized white"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/AB_Wallpaper_Light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/burst1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized-mountains-light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/d48d4ca9f67739f39d2199e30ee3ec68c24e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarizedlightstripes.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/wallpaperbetter_light.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/Screenshot_20220519_131646.png bg.png && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.selenized.white ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.selenized.white .Xdefaults
            [[ -e ~/.Xdefaults.selenized.white ]] && xrdb -load ~/.Xdefaults.selenized.white

            #### vim
            [[ -e ~/.vimrc.selenized.white ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.selenized.white .vimrc
            [[ -e ~/.gvimrc.selenized.white ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.selenized.white .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.selenized.white.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.selenized.white.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.selenized.white ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.selenized.white dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.selenized.white.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.selenized.white.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.selenized.white ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.selenized.white .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.selenized.white ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.selenized.white .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.selenized.white.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.selenized.white.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.selenized.white.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.selenized.white.yml scratchpad.yml && cd -
            ;;
        "solarized.dark")
            __printf "solarized dark"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/seed_of_life_by_lekremyelsew-d7bfnwj.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/dVMZsMn.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/BaocXcW.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/1OlbRnT.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/6aEOSOS.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/hnSuKgw.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/burst2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.solarized.dark ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.solarized.dark .Xdefaults
            [[ -e ~/.Xdefaults.solarized.dark ]] && xrdb -load ~/.Xdefaults.solarized.dark

            #### vim
            [[ -e ~/.vimrc.solarized.dark ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.solarized.dark .vimrc
            [[ -e ~/.gvimrc.solarized.dark ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.solarized.dark .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.solarized.dark.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.solarized.dark.vim init.vim && cd -

            #### gtk
            [[ -e ~/.gtkrc-2.0.solarized.dark ]] && cd ~/ && rm -f .gtkrc-2.0 && ln -s .gtkrc-2.0.solarized.dark .gtkrc-2.0

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.solarized.dark ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.solarized.dark dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.solarized.dark.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.solarized.dark.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.solarized.dark ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.solarized.dark .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.solarized.dark ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.solarized.dark .tmux.conf

            #### kitty
            [[ -e ~/.config/kitty/kitty.conf.solarized.dark ]] && cd ~/.config/kitty && rm -f kitty.conf && ln -s kitty.conf.solarized.dark kitty.conf

            #### termite
            [[ -e ~/.config/termite/config.solarized.dark ]] && cd ~/.config/termite && rm -f config && ln -s config.solarized.dark config

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.solarized.dark.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.solarized.dark.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.solarized.dark.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.solarized.dark.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.solarized.dark.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.dark.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.solarized.dark.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.dark.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi

            #### bspwm
            [[ -e ~/.config/bspwm/panel_colors.solarized.dark ]] && cd ~/.config/bspwm && rm -f panel_colors && ln -s panel_colors.solarized.dark panel_colors

            #### i3wm
            [[ -e ~/.i3/config.solarized.dark ]] && cd ~/.i3 && rm -f config && ln -s config.solarized.dark config

            #### dircolors
            # d=~/.dircolors.d/dircolors.solarized-dark
            # test -r $d && eval "$(dircolors -b $d)"
            ;;
        "solarized.light")
            __printf "solarized light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.jpg && ln -s gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            #
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/AB_Wallpaper_Light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/burst1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized-mountains-light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/d48d4ca9f67739f39d2199e30ee3ec68c24e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarizedlightstripes.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/wallpaperbetter_light.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/Screenshot_20220519_131646.png bg.png && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.solarized.light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.solarized.light .Xdefaults
            [[ -e ~/.Xdefaults.solarized.light ]] && xrdb -load ~/.Xdefaults.solarized.light

            #### vim
            [[ -e ~/.vimrc.solarized.light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.solarized.light .vimrc
            [[ -e ~/.gvimrc.solarized.light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.solarized.light .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.solarized.light.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.solarized.light.vim init.vim && cd -

            #### gtk
            [[ -e ~/.gtkrc-2.0.solarized.light ]] && cd ~/ && rm -f .gtkrc-2.0 && ln -s .gtkrc-2.0.solarized.light .gtkrc-2.0

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.solarized.light ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.solarized.light dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.solarized.light.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.solarized.light.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.solarized.light ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.solarized.light .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.solarized.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.solarized.light .tmux.conf

            #### kitty
            [[ -e ~/.config/kitty/kitty.conf.solarized.light ]] && cd ~/.config/kitty && rm -f kitty.conf && ln -s kitty.conf.solarized.light kitty.conf

            #### termite
            [[ -e ~/.config/termite/config.solarized.light ]] && cd ~/.config/termite && rm -f config && ln -s config.solarized.light config

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.solarized.light.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.solarized.light.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.solarized.light.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.solarized.light.yml scratchpad.yml && cd -

            #### xmobar
            [[ -e ~/.config/xmonad/xmobar.solarized.light.hs ]] && cd ~/.config/xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.light.hs xmobar.hs

            #### xmonad
            [[ -e ~/.config/xmonad/xmonad.solarized.light.hs ]] && cd ~/.config/xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.light.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                # xmonad --recompile
                # xmonad --restart

                ## xmonad installed from stack
                [[ -e ~/.config/xmonad ]] && cd ~/.config/xmonad && ./recompile.sh
            fi

            #### bspwm
            [[ -e ~/.config/bspwm/panel_colors.solarized.light ]] && cd ~/.config/bspwm && rm -f panel_colors && ln -s panel_colors.solarized.light panel_colors

            #### i3wm
            [[ -e ~/.i3/config.solarized.light ]] && cd ~/.i3 && rm -f config && ln -s config.solarized.light config

            #### dircolors
            # d=~/.dircolors.d/dircolors.solarized-light
            # test -r $d && eval "$(dircolors -b $d)"
            ;;
        "srcery")
            __printf "srcery"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.srcery ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.srcery .Xdefaults
            [[ -e ~/.Xdefaults.srcery ]] && xrdb -load ~/.Xdefaults.srcery

            #### vim
            [[ -e ~/.vimrc.srcery ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.srcery .vimrc
            [[ -e ~/.gvimrc.srcery ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.srcery .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.srcery.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.srcery.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.srcery ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.srcery dunstrc
            pkill dunst

            #### redshift.conf
            [[ -e ~/.config/redshift.srcery.conf ]] && cd ~/.config && rm -f redshift.conf && ln -s redshift.srcery.conf redshift.conf && cd -

            #### screenrc
            [[ -e ~/.screenrc.srcery ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.srcery .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.srcery ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.srcery .tmux.conf

            #### alacritty.yml
            [[ -e ~/.config/alacritty/alacritty.max.srcery.yml ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.max.srcery.yml alacritty.yml && cd -
            [[ -e ~/.config/alacritty/alacritty.scratchpad.srcery.yml ]] && cd ~/.config/alacritty && rm -f scratchpad.yml && ln -s alacritty.scratchpad.srcery.yml scratchpad.yml && cd -
            ;;
        "tokyo.night")
            __printf "tokyo.night"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/artur-sadlos-to-sh300-ooh-as-05i.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/chad-madden-SPIE2JfFNl0-unsplash.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-PnTznIm.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/imgur-RoWHRPi.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gruvbox/ktu605keuzx71.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/4owie6ojqz271.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/5m5kLI9.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/CjByCrG.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/Gruvbox_Lines.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/TuJrq1e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gruvbox/wall_secondary.png bg.png && cd -
            #
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mZ5NENY.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/13250.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/nature-landscapes_widewallpaper_the-perfect-nature-lscape-hdr_966.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/3b42rcb64pd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/arf6lv4kcrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/d1dq5gwyusc91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/dwrkssf0jkd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/j6f5o4x27sd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/qaecjb8q9td91.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/sejdkkpcgwd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/vaa7hengvrd91.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s Zagreb/croatia-n74ap3.jpg bg.jpg && cd -
            ####
            [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.tokyo.night ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.tokyo.night .Xdefaults
            [[ -e ~/.Xdefaults.tokyo.night ]] && xrdb -load ~/.Xdefaults.tokyo.night
            ;;
    esac

    #### duskc
    duskc run_command xrdb
}


## Function: Xrandr
##
function __xrandr()
{
    __printf "Xrandr" info

    #### xrandr output
    oldifs="${IFS}"
    IFS=$'\n'
    xrandr_connect=( $(xrandr | grep " connected" | awk '{ print $1 }') )
    xrandr_disconnect=( $(xrandr | grep " disconnected" | awk '{ print $1 }') )
    IFS="${oldifs}"

    xrandr_connect_length=${#xrandr_connect[@]}
    xrandr_disconnect_length=${#xrandr_disconnect[@]}
    __printf "xrandr_connect_length='${xrandr_connect_length}'" debug
    __printf "xrandr_disconnect_length='${xrandr_disconnect_length}'" debug

    for (( i=0; i<${#xrandr_connect[@]}; i++ ));
    do
        __printf "xrandr_connect[${i}]='${xrandr_connect[i]}'"
       [[ ${i} -eq 0 ]] && IN=${xrandr_connect[i]}
       [[ ${i} -eq 1 ]] && EXT1=${xrandr_connect[i]} && POSITION="--left-of"
       [[ ${i} -eq 2 ]] && EXT2=${xrandr_connect[i]} && POSITION="--left-of"
    done

    for (( i=0; i<${#xrandr_disconnect[@]}; i++ ));
    do
        __printf "xrandr_disconnect[${i}]='${xrandr_disconnect[i]}'" debug
    done

    #### monitor selection
    case ${xrandr_connect_length} in
        1)
            #### 1 monitor
            off=""
            for (( i=0; i<${#xrandr_disconnect[@]}; i++ ));
            do
                off="${off} --output ${xrandr_disconnect[i]} --off"
            done
            __printf "off='${off}'" debug

            #### HP EliteDesk 800 G2 SFF/8054, BIOS N01 Ver. 02.16 08/08/2016 - entd0001
            if [[ "$(hostname)" == "entd0001" ]]
            then
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --mode 1920x1200 --auto --primary ${off} \\"

                xrandr \
                    --output ${IN-} --mode 1920x1200 --auto --primary ${off}
            else
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --auto --primary ${off}"

                xrandr \
                    --output ${IN-} --auto --primary ${off}
            fi

            ;;
        2)
            #### 2 monitors

            #### Dell - dle6440, dle5570
            if [[ "$(hostname)" == "dle6440" ]] || [[ "$(hostname)" == "dle5570" ]]
            then
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --auto \\"
                __printf "    --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-}"

                xrandr \
                    --output ${IN-} --auto \
                    --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-}
            fi

            #### HP EliteBook 850 G7 Notebook - elx711804dr, elx-5cg11804dr
            if [[ "$(hostname)" == "elx711804dr" ]] || [[ "$(hostname)" == "elx-5cg11804dr" ]]
            then
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --auto \\"
                __printf "    --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-}"

                xrandr \
                    --output ${IN-} --auto \
                    --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-}
            fi

            #### HP EliteDesk 800 G2 SFF/8054, BIOS N01 Ver. 02.16 08/08/2016 - entd0001
            if [[ "$(hostname)" == "entd0001" ]]
            then
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --mode 1920x1200 \\"
                __printf "    --output ${EXT1-} --mode 1920x1080 --primary ${POSITION-} ${IN-}"

                xrandr \
                    --output ${IN-} --mode 1920x1200 \
                    --output ${EXT1-} --mode 1920x1080 --primary ${POSITION-} ${IN-}
            fi
            ;;
        3)
            #### 3 monitors

            #### Dell - dle6440, dle5570
            if [[ "$(hostname)" == "dle6440" ]] || [[ "$(hostname)" == "dle5570" ]]
            then
                __printf "# xrandr \\"
                __printf "    --output ${IN-} --auto \\"
                __printf "    --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \\"
                __printf "    --output ${EXT2-} --auto ${POSITION-} ${EXT1-}"

                xrandr \
                    --output ${IN-} --auto \
                    --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \
                    --output ${EXT2-} --auto ${POSITION-} ${EXT1-}
            fi

            #### HP EliteBook 850 G7 Notebook - elx711804dr, elx-5cg11804dr
            if [[ "$(hostname)" == "elx711804dr" ]] || [[ "$(hostname)" == "elx-5cg11804dr" ]]
            then
                if [[ "${EXT1-}" == "DP-1-1" ]] && [[ "${EXT2-}" == "DP-1-2" ]]
                then
                    #### ETK
                    __printf " xrandr \\"
                    __printf "--output ${IN-} --auto \\"
                    __printf "    --output ${EXT1-} --mode 1920x1080 --primary ${POSITION-} ${IN-} \\"
                    __printf "    --output ${EXT2-} --mode 1920x1080 ${POSITION-} ${EXT1-}"

                    xrandr \
                        --output ${IN-} --auto \
                        --output ${EXT1-} --mode 1920x1080 --primary ${POSITION-} ${IN-} \
                        --output ${EXT2-} --mode 1920x1080 ${POSITION-} ${EXT1-}
                else
                    #### Home
                    __printf " xrandr \\"
                    __printf "--output ${IN-} --auto \\"
                    __printf "    --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-} \\"
                    __printf "    --output ${EXT2-} --mode 1920x1200 ${POSITION-} ${EXT1-}"

                    xrandr \
                        --output ${IN-} --auto \
                        --output ${EXT1-} --mode 1920x1200 --primary ${POSITION-} ${IN-} \
                        --output ${EXT2-} --mode 1920x1200 ${POSITION-} ${EXT1-}
                fi
            fi
            ;;
    esac
}


## Function: Background
##
function __background()
{
    __printf "Background" info

    #### ELX2
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_blue.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_green.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_purple_blue.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_purple.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_red_orange.png

    #### ELX3
    # feh --bg-scale ~/wallpapers/elx30/wallpaper_blue_official.png
    # feh --bg-scale ~/wallpapers/elx30/wallpaper_green_official.png
    # feh --bg-scale ~/wallpapers/elx30/wallpaper_maintenance.png
    # feh --bg-scale ~/wallpapers/elx30/wallpaper_purple_official.png
    ####
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_3_3_baseband.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_3_3_bdgs.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_3_3.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_3_3_research.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_blue_official.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_green_official.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_maintenance.png
    # feh --bg-scale ~/wallpapers/elx33/wallpaper_purple_official.png

    #### ELX4
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_ericssonlinux_blue.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_ericssonlinux_green.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_ericssonlinux_purple.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_research_blue.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_research_green.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_4_0_research_purple.png
    # feh --bg-scale ~/wallpapers/elx40/wallpaper_blue_official.png

    #### Default
    [[ -e ~/wallpapers/bg.jpg ]] && feh --bg-scale ~/wallpapers/bg.jpg
    [[ -e ~/wallpapers/bg.png ]] && feh --bg-scale ~/wallpapers/bg.png
}


## Function: Keyboard
##
function __keyboard()
{
    __printf "Keyboard" info

    #### keyboard
    # setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch,ctrl:nocaps' 'hr(us)'
    setxkbmap -model pc105 -option 'eurosign:e,lv3:ralt_switch,compose:caps' 'hr(us)'
    # setxkbmap -rules evdev -model evdev -option 'eurosign:e,grp:alt_shift_toggle,lv3:ralt_switch:ctrl:nocaps' -layout 'hr(us),de'
    # setxkbmap -rules evdev -model pc105 -option 'eurosign:e,grp:alt_shift_toggle,lv3:ralt_switch,ctrl:nocaps,compose:rctrl' -layout 'hr(us),de(qwerty)'
    # setxkbmap -rules evdev -model pc105 -option 'eurosign:e,grp:alt_shift_toggle,lv3:ralt_switch,ctrl:nocaps,compose:rwin' -layout 'hr(us)'
    # setxkbmap -rules evdev -model pc105 -option 'eurosign:e,grp:alt_shift_toggle,lv3:ralt_switch,ctrl:nocaps,compose:caps' -layout 'hr(us)'
}


###############################################################################
## Main
###############################################################################

#### Banner
__banner

#### Command Line
__xrandr_flag=""
__theme_flag=""

while getopts "hxs:l:" opt;
do
    case ${opt} in
        h)
            __help
            exit ${EXIT_OK}
            ;;
        x)
            __xrandr_flag="true"
            ;;
        s)
            __theme_flag="true"
            THEME=${OPTARG}
            ;;
        l)
            LOG_ENABLED="true"
            LOG_FILE=${OPTARG}
            __printf "enabling logging to '${LOG_FILE}' ..." info
            ;;
        :)
            __printf "Option -${opt} requires an argument!" error
            exit ${EXIT_ERROR}
            ;;
    esac
done

#### This tells getopts to move on to the next argument.
shift $((OPTIND-1))

if [[ -z "${__xrandr_flag-}" ]] && [[ -z "${__theme_flag}" ]]
then
    __printf "Missing arguments!" error
    __help
    exit ${EXIT_ERROR}
else
    if [[ "${__xrandr_flag-}" == "true" ]]
    then
        #### xrandr
        __xrandr
        #### background
        __background
        #### keyboard
        __keyboard
    fi

    if [[ "${__theme_flag-}" == "true" ]]
    then
        #### Theme
        __theme ${THEME}
        #### keyboard
        __keyboard
    fi
fi

#### Done
__printf "done!"

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
