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
#       CREATED: 2018-07-13 18:56:32
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
    __printf "  -s <dark|light>   Solarized dark | Solarized light"
    __printf "  -l <file>  Log to <file>"
    __printf "Examples:"
    __printf "   ${0} -x"
    __printf "   ${0} -s dark"
    __printf "   ${0} -s light"
}


## Function: Solarized
##
function __solarized()
{
    __printf "Solarized" info

    #### arg
    solarized=${1}
    __printf "solarized='${solarized}'" debug

    #### set solarized
    case ${solarized} in
        "dark")
            __printf "solarized dark"

            #### xmobar
            [[ -e ~/.xmonad/xmobar.solarized.dark.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.dark.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.solarized.dark.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.dark.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad
                cd ~/data/cabal/xmonad && cabal exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal exec -- xmonad --restart
            else
                ## xmonad installed from package
                xmonad --recompile
                xmonad --restart
            fi

            #### background
            cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized_mountains_by_9beat7-d8rkbit.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s seed_of_life_by_lekremyelsew-d7bfnwj.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s dVMZsMn.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s TVDBMOt.png solarized.png && cd -
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s BaocXcW.png solarized.png && cd -
            ##
            feh --bg-scale ~/wallpapers/solarized/solarized.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.solarized.dark ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.solarized.dark .Xdefaults
            [[ -e ~/.Xdefaults.solarized.dark ]] && xrdb -load ~/.Xdefaults.solarized.dark

            #### vim
            [[ -e ~/.vimrc.solarized.dark ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.solarized.dark .vimrc
            [[ -e ~/.gvimrc.solarized.dark ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.solarized.dark .gvimrc

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

            #### dircolors
            # d=~/.dircolors.d/dircolors.solarized-dark
            # test -r $d && eval "$(dircolors -b $d)"
            ;;
        "light")
            __printf "solarized light"

            #### xmobar
            [[ -e ~/.xmonad/xmobar.solarized.light.hs ]] && cd ~/.xmonad && rm -f xmobar.hs && ln -s xmobar.solarized.light.hs xmobar.hs

            #### xmonad
            [[ -e ~/.xmonad/xmonad.solarized.light.hs ]] && cd ~/.xmonad && rm -f xmonad.hs && ln -s xmonad.solarized.light.hs xmonad.hs

            if [[ -d ~/data/cabal/xmonad ]]
            then
                ## custom compiled xmonad
                cd ~/data/cabal/xmonad && cabal exec -- xmonad --recompile
                cd ~/data/cabal/xmonad && cabal exec -- xmonad --restart
            else
                ## xmonad installed from package
                xmonad --recompile
                xmonad --restart
            fi

            #### background
            cd ~/wallpapers/solarized && rm -f solarized.png && ln -s solarized-mountains-light.png solarized.png
            # cd ~/wallpapers/solarized && rm -f solarized.png && ln -s AB_Wallpaper_Light.png solarized.png
            ##
            feh --bg-scale ~/wallpapers/solarized/solarized.png

            #### Xdefaults
            [[ -e ~/.Xdefaults.solarized.light ]] && cd ~/ && rm -f .Xdefaults && ln -s .Xdefaults.solarized.light .Xdefaults
            [[ -e ~/.Xdefaults.solarized.light ]] && xrdb -load ~/.Xdefaults.solarized.light

            #### vim
            [[ -e ~/.vimrc.solarized.light ]] && cd ~/ && rm -f .vimrc && ln -s .vimrc.solarized.light .vimrc
            [[ -e ~/.gvimrc.solarized.light ]] && cd ~/ && rm -f .gvimrc && ln -s .gvimrc.solarized.light .gvimrc

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
        __printf "xrandr_connect[${i}]='${xrandr_connect[i]}'" debug
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
            __printf "    --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \\"
            __printf "    --output ${EXT2-} --auto ${POSITION-} ${EXT1-}"

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \
                --output ${EXT2-} --auto ${POSITION-} ${EXT1-}
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
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_blue_official.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_green_official.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_maintenance.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_purple_official.png

    #### Nature
    # feh --bg-scale ~/wallpapers/nature/viper_1600x900.jpg
    # feh --bg-scale ~/wallpapers/nature/37590-greenz2.jpg
    # feh --bg-scale ~/wallpapers/nature/15227.jpg

    #### Gray
    # feh --bg-scale ~/wallpapers/gray/minimalistic-gray-2560x1440-wallpaper-2109511.png
    # feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg
    # feh --bg-scale ~/wallpapers/gray/grey-popular-wallpaper-backgrounds-filter-alexander-room-surface-resolutions-77891.jpg
    # feh --bg-scale ~/wallpapers/gray/defruwallpaper1920x1200eo2.jpg

    #### Green
    # feh --bg-scale ~/wallpapers/green/ky7tee2.jpg
    # feh --bg-scale ~/wallpapers/green/lines_spots_color_texture_50390_3840x2400.jpg

    #### Solarized
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s solarized_mountains_by_9beat7-d8rkbit.png solarized.png
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s seed_of_life_by_lekremyelsew-d7bfnwj.png solarized.png
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s dVMZsMn.png solarized.png
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s TVDBMOt.png solarized.png
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s BaocXcW.png solarized.png
    #### Solarized Light
    # [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s solarized-mountains-light.png solarized.png
    [[ ! -e ~/wallpapers/solarized/solarized.png ]] && cd ~/wallpapers/solarized && ln -s AB_Wallpaper_Light.png solarized.png
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
__solarized_flag=""

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
            __solarized_flag="true"
            SOLARIZED=${OPTARG}
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

if [[ -z "${__xrandr_flag-}" ]] && [[ -z "${__solarized_flag}" ]]
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

    if [[ "${__solarized_flag-}" == "true" ]]
    then
        #### solarized
        __solarized ${SOLARIZED}
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