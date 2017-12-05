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
#  ORGANIZATION: RWCE ETK ICT Development
#       CREATED: 2016-04-07 06:14:49
#      REVISION: ---
#===============================================================================

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

## HP 8560w
# IN="LVDS-0"
# EXT1="VGA-0"
# EXT2="VGA-0"

## HP nc6400
# IN="LVDS-1"
# EXT1="VGA-1"

## Dell E6440
# IN="eDP-1"
#
# EXT1="HDMI-2"
# EXT2="HDMI-3"
#
# EXT1="VGA-1"
# EXT2="VGA-2"

## Dell E5570
IN="eDP1"
#
# EXT1="HDMI1"
# EXT2="HDMI2"
#
EXT1="DP3"
EXT2="DP4"

## Position
# POSITION="--left-of"
POSITION="--right-of"

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
    __printf "${BANNER}" header
}


#### Function: Help
####
__help()
{
    __printf "Usage: ${0} [ -h | -x ]"
    __printf "  -h         Help"
    __printf "  -x         Xrandr"
    __printf "  -l <file>  Log to <file>"
    __printf "Examples:"
    __printf "   ${0} -x"
}


## Function: Xrandr
##
function __xrandr()
{
    __printf "Xrandr" header

    xrandr --auto

    if [[ $(xrandr | grep "${EXT1-} disconnected" | wc -l) -eq 1 ]]
    then
        __printf "'${EXT1-}' disconnected" info

        if [[ $(xrandr | grep "${EXT2-} disconnected" | wc -l) -eq 1 ]]
        then
            ## IN   - on
            ## EXT1 - off
            ## EXT2 - off
            ##
            __printf "'${EXT1-}' off, '${EXT2-}' off" info

            xrandr \
                --output ${IN-} --auto --primary \
                --output ${EXT1-} --off \
                --output ${EXT2-} --off
        elif [[ $(xrandr | grep "${EXT2-} connected" | wc -l) -eq 1 ]]
        then
            ## IN   - on
            ## EXT1 - off
            ## EXT2 - on
            ##
            __printf "'${EXT1-}' off, '${EXT2-}' on" info

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --off \
                --output ${EXT2-} --auto --primary ${POSITION-} ${IN-}
        else
            ## IN   - on
            ## EXT1 - off
            ## EXT2 - not defined
            ##
            __printf "'${EXT1-}' off" info

            xrandr \
                --output ${IN-} --auto --primary \
                --output ${EXT1-} --off
        fi
    else
        __printf "'${EXT1}' connected" info

        if [[ $(xrandr | grep "${EXT2-} disconnected" | wc -l) -eq 1 ]]
        then
            ## IN   - on
            ## EXT1 - on
            ## EXT2 - off
            ##
            __printf "'${EXT1-}' on, '${EXT2-}' off" info

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \
                --output ${EXT2-} --off
        elif [[ $(xrandr | grep "${EXT2-} connected" | wc -l) -eq 1 ]]
        then
            ## IN   - on
            ## EXT1 - on
            ## EXT2 - on
            ##
            __printf "'${EXT1-}' on, '${EXT2-}' on" info

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --auto --primary ${POSITION-} ${IN-} \
                --output ${EXT2-} --auto ${POSITION-} ${EXT1-}
        else
            ## IN   - on
            ## EXT1 - on
            ## EXT2 - not defined
            ##
            __printf "'${EXT1-}' on" info

            xrandr \
                --output ${IN-} --auto \
                --output ${EXT1-} --auto --primary ${POSITION-} ${IN-}
        fi
    fi
}


## Function: Background
##
function __background()
{
    __printf "Background" header

    ## background
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_blue.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_green.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_purple_blue.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_purple.png
    # feh --bg-scale ~/wallpapers/elx2/elx_wallpaper_red_orange.png

    # feh --bg-scale ~/wallpapers/elx3/wallpaper_blue_official.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_green_official.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_maintenance.png
    # feh --bg-scale ~/wallpapers/elx3/wallpaper_purple_official.png
    #
    # feh --bg-scale ~/wallpapers/hr/zadar_fosa_20150623_1.jpg
    # feh --bg-scale ~/wallpapers/hr/zadar_fosa_20150627_1.jpg
    #
    # feh --bg-scale ~/wallpapers/nature/viper_1600x900.jpg
    # feh --bg-scale ~/wallpapers/nature/37590-greenz2.jpg
    # feh --bg-scale ~/wallpapers/nature/15227.jpg
    #
    # feh --bg-scale ~/wallpapers/gray/minimalistic-gray-2560x1440-wallpaper-2109511.png
    feh --bg-scale ~/wallpapers/gray/Minimalistic_gray_colors_2560x1600.jpg
    # feh --bg-scale ~/wallpapers/gray/grey-popular-wallpaper-backgrounds-filter-alexander-room-surface-resolutions-77891.jpg
    #
    # feh --bg-scale ~/wallpapers/space/night_stars_Constellation_skyscapes_starfield_night_sky_1920x1080.jpg
    #
    # feh --bg-scale ~/wallpapers/20170505.164342.jpg
}


## Function: Keyboard
##
function __keyboard()
{
    __printf "Keyboard" header

    ## keyboard
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

while getopts "hxl:" opt;
do
    case ${opt} in
        h)
            __help
            exit ${EXIT_OK}
            ;;
        x)
            __xrandr_flag="true"
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

if [ -z "${__xrandr_flag-}" ]
then
    __printf "Missing arguments!" error
    __help
    exit ${EXIT_ERROR}
else
    ## xrandr
    __xrandr
    ## background
    __background
    ## keyboard
    __keyboard
fi

#### Done
__printf "done!"

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
