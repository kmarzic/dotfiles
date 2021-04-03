#!/bin/bash
#===============================================================================
#
#          FILE: screen_toogle.sh
#
#         USAGE: ./screen_toogle.sh [ -h | -x | -s <theme> ]
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2020-11-28 08:56:32
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
    __printf "   ${0} -s dracula"
    __printf "   ${0} -s monokai"
    __printf "   ${0} -s nord"
    __printf "   ${0} -s papercolor.light"
    __printf "   ${0} -s solarized.dark"
    __printf "   ${0} -s solarized.light"
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
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/6137182928_3fa8c655e4_o.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/day-dive-hd-wallpaper-1680x1050.jpeg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/4543.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/20-2.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/1504.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nature/NATURE-SyltSunset_1600x1200.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/o1odnceofk351.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/viper_1600x900.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/x2dezef8sn351.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/mguoNgT.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/02enw77lod861.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s nature/gpwq7rbsjof61.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s nature/r9y8ud7z6jf61.jpg bg.jpg && cd -
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s gray/minimalistic-gray-2560x1440-wallpaper-2109511.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/170583-gorgerous-light-gray-background-2000x2000.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/gXswE4.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s gray/kNse8Ue.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s gray/Minimalistic_gray_colors_2560x1600.jpg bg.jpg && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.ansi ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.ansi alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.ansi.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.ansi.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.ansi.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.ansi.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi
            ;;
        "base16-atelier-lakeside-light")
            __printf "base16-atelier-lakeside-light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.png && ln -s nature/20-2.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nature/13250.jpg bg.jpg && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.base16-atelier-lakeside-light ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.base16-atelier-lakeside-light alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.base16-atelier-lakeside-light.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.base16-atelier-lakeside-light.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.base16-atelier-lakeside-light.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.base16-atelier-lakeside-light.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi
            ;;
        "base16-google-light")
            __printf "base16-google-light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.png && ln -s nature/20-2.jpg bg.jpg && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s nature/13250.jpg bg.jpg && cd -
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
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s dracula/base.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s dracula/dracula.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s dracula/dracula-purplish.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.jpg && ln -s dracula/Ycjig3q.jpg bg.jpg && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.dracula ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.dracula alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.dracula.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.dracula.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.dracula.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.dracula.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi
            ;;
        "monokai")
            __printf "monokai"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.jpg && ln -s monokai/minimal-minimalist-nature-sunrise-river-mountains-trees-8k-wallpaper-1920x1080.jpg bg.jpg && cd -
            cd ~/wallpapers && rm -f bg.jpg && ln -s monokai/4TqeQei.jpg bg.jpg && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.monokai ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.monokai alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.monokai.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.monokai.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.monokai.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.monokai.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi
            ;;
        "nord")
            __printf "nord"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            cd ~/wallpapers && rm -f bg.png && ln -s nord/2560x1080.png bg.png && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.nord ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.nord alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.nord.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.nord.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.nord.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.nord.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi
            ;;
        "papercolor.light")
            __printf "papercolor light"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized-mountains-light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/AB_Wallpaper_Light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/d48d4ca9f67739f39d2199e30ee3ec68c24e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarizedlightstripes.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/wallpaperbetter.jpg.jpg bg.jpg && cd -
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
            [[ -e ~/.tmux.conf.solarized.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.papercolor.light .tmux.conf
            ;;
        "solarized.dark")
            __printf "solarized dark"

            #### Background
            [[ -e ~/wallpapers/bg.jpg ]] && rm -f ~/wallpapers/bg.jpg
            [[ -e ~/wallpapers/bg.png ]] && rm -f ~/wallpapers/bg.png
            ####
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized_mountains_by_9beat7-d8rkbit.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/seed_of_life_by_lekremyelsew-d7bfnwj.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/dVMZsMn.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/TVDBMOt.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/BaocXcW.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/1OlbRnT.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/6aEOSOS.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/hnSuKgw.png bg.png && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.solarized.dark ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.solarized.dark alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.solarized.dark.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.dark.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.solarized.dark.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.dark.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
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
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarized-mountains-light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/AB_Wallpaper_Light.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/green-texture-wallpaper.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/d48d4ca9f67739f39d2199e30ee3ec68c24e.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/solarizedlightstripes.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA1.png bg.png && cd -
            cd ~/wallpapers && rm -f bg.png && ln -s solarized/ErSSyA2.png bg.png && cd -
            # cd ~/wallpapers && rm -f bg.png && ln -s solarized/wallpaperbetter.jpg.jpg bg.jpg && cd -
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
            [[ -e ~/.config/alacritty/alacritty.yml.solarized.light ]] && cd ~/.config/alacritty && rm -f alacritty.yml && ln -s alacritty.yml.solarized.light alacritty.yml && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.solarized.light.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.light.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.solarized.light.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.light.hs xmonad.hs

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
                cd ~/.xmonad && ./recompile.sh
            fi

            #### bspwm
            [[ -e ~/.config/bspwm/panel_colors.solarized.light ]] && cd ~/.config/bspwm && rm -f panel_colors && ln -s panel_colors.solarized.light panel_colors

            #### i3wm
            [[ -e ~/.i3/config.solarized.light ]] && cd ~/.i3 && rm -f config && ln -s config.solarized.light config

            #### dircolors
            # d=~/.dircolors.d/dircolors.solarized-light
            # test -r $d && eval "$(dircolors -b $d)"
            ;;
    esac
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
       [[ ${i} -eq 1 ]] && EXT1=${xrandr_connect[i]} && POSITION="--right-of"
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

            __printf "# xrandr \\"
            __printf "    --output ${IN-} --auto --primary ${off}"

            xrandr \
                --output ${IN-} --auto --primary ${off}
            ;;
        2)
            #### 2 monitors
            __printf "# xrandr \\"
            __printf "    --output ${IN-} --auto \\"
            __printf "    --output ${EXT1-} --auto --primary ${POSITION-} ${IN-}"

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --auto --primary ${POSITION-} ${IN-}
            ;;
        3)
            #### 3 monitors
            __printf "# xrandr \\"
            __printf "    --output ${IN-} --auto \\"
            __printf "    --output ${EXT2-} --auto --primary ${POSITION-} ${IN-} \\"
            __printf "    --output ${EXT1-} --auto ${POSITION-} ${EXT2-}"

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT2-} --auto --primary ${POSITION-} ${IN-} \
                --output ${EXT1-} --auto ${POSITION-} ${EXT2-}
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
