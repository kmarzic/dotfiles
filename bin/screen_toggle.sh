#!/bin/bash
#===============================================================================
#
#          FILE: screen_toogle.sh
#
#         USAGE: ./screen_toogle.sh
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2020-02-10 18:56:32
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
    __printf "Usage: ${0} [ -h | -x ]"
    __printf "  -h                Help"
    __printf "  -x                Xrandr"
    __printf "  -s <theme>        Theme"
    __printf "  -l <file>  Log to <file>"
    __printf "Examples:"
    __printf "   ${0} -x"
    __printf "   ${0} -s ansi"
    __printf "   ${0} -s zenburn"
    __printf "   ${0} -s blue"
    __printf "   ${0} -s green"
    __printf "   ${0} -s dark"
    __printf "   ${0} -s light"
}


## Function: Theme
##
function __theme()
{
    __printf "Theme" info

    #### arg
    theme=${1}
    __printf "theme='${theme}'" debug

    #### set solarized
    case ${theme} in
        "ansi")
            __printf "ansi"

            #### Background
            # feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg
            # feh --bg-scale ~/wallpapers/nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg
            # feh --bg-scale ~/wallpapers/nature/day-dive-hd-wallpaper-1680x1050.jpeg
            # feh --bg-scale ~/wallpapers/nature/4543.jpg
            # feh --bg-scale ~/wallpapers/nature/20-2.jpg
            # feh --bg-scale ~/wallpapers/nature/1504.jpg
            # feh --bg-scale ~/wallpapers/gray/minimalistic-gray-2560x1440-wallpaper-2109511.png
            # feh --bg-scale ~/wallpapers/gray/170583-gorgerous-light-gray-background-2000x2000.jpg
            # feh --bg-scale ~/wallpapers/gray/gXswE4.jpg
            # feh --bg-scale ~/wallpapers/gray/kNse8Ue.png
            feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg

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

            #### screenrc
            [[ -e ~/.screenrc.ansi ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.ansi .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.ansi ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.ansi .tmux.conf

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
                xmonad --recompile
                xmonad --restart
            fi
            ;;
        "zenburn")
            __printf "zenburn"

            #### Background
            feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg

            #### Xdefaults
            [[ -e ~/.Xdefaults.zenburn ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.zenburn .Xdefaults
            [[ -e ~/.Xdefaults.zenburn ]] && xrdb -load ~/.Xdefaults.zenburn

            #### vim
            [[ -e ~/.vimrc.zenburn ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.zenburn .vimrc
            [[ -e ~/.gvimrc.zenburn ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.zenburn .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.zenburn.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.zenburn.vim init.vim && cd -

            #### dunstrc
            [[ -e ~/.config/dunst/dunstrc.zenburn ]] && cd ~/.config/dunst && rm -f dunstrc && ln -s dunstrc.zenburn dunstrc
            pkill dunst

            #### screenrc
            [[ -e ~/.screenrc.zenburn ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.zenburn .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.zenburn ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.zenburn .tmux.conf

            #### xmobar
            [[ -e ~/.xmonad/xmobar.zenburn.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.zenburn.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.zenburn.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.zenburn.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                xmonad --recompile
                xmonad --restart
            fi
            ;;
        "blue")
            __printf "blue"

            #### Background
            feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg

            #### Xdefaults
            [[ -e ~/.Xdefaults.blue ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.blue .Xdefaults
            [[ -e ~/.Xdefaults.blue ]] && xrdb -load ~/.Xdefaults.blue

            #### vim
            [[ -e ~/.vimrc.blue ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.blue .vimrc
            [[ -e ~/.gvimrc.blue ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.blue .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.blue.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.blue.vim init.vim && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.blue.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.blue.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.blue.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.blue.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                xmonad --recompile
                xmonad --restart
            fi
            ;;
        "green")
            __printf "green"

            #### Background
            feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg

            #### Xdefaults
            [[ -e ~/.Xdefaults.green ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.green .Xdefaults
            [[ -e ~/.Xdefaults.green ]] && xrdb -load ~/.Xdefaults.green

            #### vim
            [[ -e ~/.vimrc.green ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.green .vimrc
            [[ -e ~/.gvimrc.green ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.green .gvimrc

            #### neovim
            [[ -e ~/.config/nvim/init.green.vim ]] && cd ~/.config/nvim && rm -f init.vim && ln -s init.green.vim init.vim && cd -

            #### xmobar
            [[ -e ~/.xmonad/xmobar.green.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.green.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.green.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.green.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad - used sandbox
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal v1-exec -- xmonad --restart
            else
                ## xmonad installed from package
                xmonad --recompile
                xmonad --restart
            fi
            ;;
        "dark")
            __printf "solarized dark"

            #### Background
            cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized_mountains_by_9beat7-d8rkbit.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s seed_of_life_by_lekremyelsew-d7bfnwj.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s dVMZsMn.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s TVDBMOt.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s BaocXcW.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s green-texture-wallpaper.png solarized.png && cd -
            ##
            feh --bg-scale ~/wallpapers/solarized/solarized.png

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

            #### screenrc
            [[ -e ~/.screenrc.solarized.dark ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.solarized.dark .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.solarized.dark ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.solarized.dark .tmux.conf

            #### kitty
            [[ -e ~/.config/kitty/kitty.conf.solarized.dark ]] && cd ~/.config/kitty && rm -f kitty.conf && ln -s kitty.conf.solarized.dark kitty.conf

            #### termite
            [[ -e ~/.config/termite/config.solarized.dark ]] && cd ~/.config/termite && rm -f config && ln -s config.solarized.dark config

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
                xmonad --recompile
                xmonad --restart
            fi

            #### bspwm
            [[ -e ~/.config/bspwm/panel_colors.solarized.dark ]] && cd ~/.config/bspwm && rm -f panel_colors && ln -s panel_colors.solarized.dark panel_colors

            #### i3wm
            [[ -e ~/.i3/config.solarized.dark ]] && cd ~/.i3 && rm -f config && ln -s config.solarized.dark config

            #### dircolors
            # d=~/.dircolors.d/dircolors.solarized-dark
            # test -r $d && eval "$(dircolors -b $d)"
            ;;
        "light")
            __printf "solarized light"

            #### Background
            cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized-mountains-light.png solarized.png
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s AB_Wallpaper_Light.png solarized.png
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s green-texture-wallpaper.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s d48d4ca9f67739f39d2199e30ee3ec68c24e.png solarized.png && cd -
            ##
            feh --bg-scale ~/wallpapers/solarized/solarized.png

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

            #### screenrc
            [[ -e ~/.screenrc.solarized.light ]] && cd ~/ && rm -f .screenrc && ln -s .screenrc.solarized.light .screenrc

            #### tmux.conf
            [[ -e ~/.tmux.conf.solarized.light ]] && cd ~/ && rm -f .tmux.conf && ln -s .tmux.conf.solarized.light .tmux.conf

            #### kitty
            [[ -e ~/.config/kitty/kitty.conf.solarized.light ]] && cd ~/.config/kitty && rm -f kitty.conf && ln -s kitty.conf.solarized.light kitty.conf

            #### termite
            [[ -e ~/.config/termite/config.solarized.light ]] && cd ~/.config/termite && rm -f config && ln -s config.solarized.light config

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
                xmonad --recompile
                xmonad --restart
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

    #### Nature
    # feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg
    # feh --bg-scale ~/wallpapers/nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg
    # feh --bg-scale ~/wallpapers/nature/day-dive-hd-wallpaper-1680x1050.jpeg
    # feh --bg-scale ~/wallpapers/nature/4543.jpg
    # feh --bg-scale ~/wallpapers/nature/20-2.jpg
    # feh --bg-scale ~/wallpapers/nature/1504.jpg
    # feh --bg-scale ~/wallpapers/gray/minimalistic-gray-2560x1440-wallpaper-2109511.png
    # feh --bg-scale ~/wallpapers/gray/170583-gorgerous-light-gray-background-2000x2000.jpg
    # feh --bg-scale ~/wallpapers/gray/gXswE4.jpg
    # feh --bg-scale ~/wallpapers/gray/kNse8Ue.png
    # feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg

    #### Gray
    # feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg
    # feh --bg-scale ~/wallpapers/gray/grey-popular-wallpaper-backgrounds-filter-alexander-room-surface-resolutions-77891.jpg
    # feh --bg-scale ~/wallpapers/gray/defruwallpaper1920x1200eo2.jpg
    # feh --bg-scale ~/wallpapers/gray/ky7tee2.jpg
    # feh --bg-scale ~/wallpapers/gray/kNse8Ue.png
    # feh --bg-scale ~/wallpapers/gray/minimalistic-gray-2560x1440-wallpaper-2109511.png
    # feh --bg-scale ~/wallpapers/gray/113243-most-popular-light-gray-background-2000x2000-iphone.jpg
    # feh --bg-scale ~/wallpapers/gray/113256-popular-light-gray-background-1920x1200-for-windows.jpg
    # feh --bg-scale ~/wallpapers/gray/170578-popular-light-gray-background-1920x1080-for-mobile-hd.jpg
    # feh --bg-scale ~/wallpapers/gray/170583-gorgerous-light-gray-background-2000x2000.jpg

    #### Ansi
    # feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg
    # feh --bg-scale ~/wallpapers/nature/selkirk-docks-hd-wallpaper-1680x1050.jpeg
    # feh --bg-scale ~/wallpapers/nature/day-dive-hd-wallpaper-1680x1050.jpeg
    # feh --bg-scale ~/wallpapers/nature/4543.jpg
    # feh --bg-scale ~/wallpapers/nature/20-2.jpg
    # feh --bg-scale ~/wallpapers/nature/1504.jpg
    # feh --bg-scale ~/wallpapers/gray/gXswE4.jpg
    # feh --bg-scale ~/wallpapers/gray/kNse8Ue.png

    #### Blue
    # feh --bg-scale ~/wallpapers/nature/6137182928_3fa8c655e4_o.jpg

    #### Green
    # feh --bg-scale ~/wallpapers/green/lines_spots_color_texture_50390_3840x2400.jpg

    #### Solarized
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized_mountains_by_9beat7-d8rkbit.png solarized.png && cd -
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s seed_of_life_by_lekremyelsew-d7bfnwj.png solarized.png && cd -
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s dVMZsMn.png solarized.png && cd -
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s TVDBMOt.png solarized.png && cd -
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s BaocXcW.png solarized.png && cd -
    #### Solarized Light
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized-mountains-light.png solarized.png && cd -
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s AB_Wallpaper_Light.png solarized.png && cd -
    #### Solarized Dark
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && rm -f solarized.png && ln -s green-texture-wallpaper.png solarized.png && cd -
    ####
    feh --bg-scale ~/wallpapers/solarized/solarized.png
    # feh --bg-tile ~/wallpapers/solarized/solarizedlightstripes.png
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
