#!/usr/bin/env bash
#===============================================================================
#
#          FILE: dwm.status.sh
#
#         USAGE: ./dwm.status.sh [ -h | -a <dwmblock> | -s | -t <theme> ]
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2022-12-21 14:14:32
#      REVISION: ---
#===============================================================================

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### Banner
BANNER="DWM Status"

#### Debug
DEBUG=0 # debug is off
# DEBUG=1 # debug is on

## Exit
EXIT_OK=0
EXIT_ERROR=1

#### variables
OS=$(uname -s)
DEFAULT_SPACES=7
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"
STATUSCOLOR=1
GLYPH_BATTERY="^r0,7,2,4^^r2,4,22,10^^c#000000^^r3,5,20,8^^c#ffffff^^r10,5,13,8^^d^^f24^"


###############################################################################
## functions
###############################################################################

#### Function: Printf
####
function __printf()
{
    ## defining colors for outputs
    __RED='\033[31m'
    __GREEN='\033[32m'
    __YELLOW='\033[33m'
    __BOLD='\033[1m'
    __NORMAL='\033[m'

    [ "${3-}" = "nolb" ] && ECHOSWITCH="-ne" || ECHOSWITCH="-e"

    if [[ ! -z ${2-} && ! -z ${1-} ]]
    then
        case ${2} in
            error)
                echo -e "${__RED}${1}${__NORMAL}" >&2
                ;;
            info)
                echo ${ECHOSWITCH} "${__YELLOW}${1}${__NORMAL}"
                ;;
            success)
                echo -e "${__GREEN}${1}${__NORMAL}"
                ;;
            header)
                echo -e "${__BOLD}${1}${__NORMAL}"
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

#### Function: Banner
####
function __banner()
{
    __printf "${BANNER}" info
}

#### Function: Help
####
__help()
{
    __printf "Usage: ${0} [ -h | -a <dwmblock> | -s | -t <theme> ]"
    __printf "  -h                Help"
    __printf "  -d <dwmblock>     DWM Status - async"
    __printf "  -a                DWM Status - sync"
    __printf "  -s <theme>        Theme"
    __printf "  -l <file>         Log to <file>"
    __printf ""
    __printf "Themes:"
    __printf "   ${0} -t ansi"
    __printf "   ${0} -t base16-atelier-lakeside-light"
    __printf "   ${0} -t base16-google-light"
    __printf "   ${0} -t base16-gruvbox-dark-soft"
    __printf "   ${0} -t doom-one"
    __printf "   ${0} -t dracula"
    __printf "   ${0} -t everforest"
    __printf "   ${0} -t gruvbox.dark"
    __printf "   ${0} -t monokai"
    __printf "   ${0} -t nord"
    __printf "   ${0} -t papercolor.light"
    __printf "   ${0} -t selenized.dark"
    __printf "   ${0} -t selenized.light"
    __printf "   ${0} -t solarized.dark"
    __printf "   ${0} -t solarized.light"
    __printf "   ${0} -t srcery"
    __printf ""
    __printf "Examples:"
    __printf "${0} -s -t gruvbox.dark"
    __printf "${0} -a sb-load    -t gruvbox.dark"
    __printf "${0} -a sb-memory  -t gruvbox.dark"
    __printf "${0} -a sb-battery -t gruvbox.dark"
    __printf "${0} -a sb-network -t gruvbox.dark"
    __printf "${0} -a sb-time    -t gruvbox.dark"
}

#### Function: Theme environment
####
function __theme()
{
    #### arg
    theme="${1}"
    __printf "theme='${theme}'" debug

    #### ansi - default
    CYAN='^c#00ffff^'
    GREEN='^c#00d700^'
    ORANGE='^c#d78700^'
    PINK='^c#d787af^'
    PURPLE='^c#d700af^'
    RED='^c#ff0000^'
    YELLOW='^c#ffff00^'
    NORMAL='^c#bbbbbb^'

    #### themes
    case ${theme} in
        "ansi")
            __printf "ansi" debug
            CYAN='^c#00ffff^'
            GREEN='^c#00d700^'
            ORANGE='^c#d78700^'
            PINK='^c#d787af^'
            PURPLE='^c#d700af^'
            RED='^c#ff0000^'
            YELLOW='^c#ffff00^'
            NORMAL='^c#bbbbbb^'
            ;;
        "base16-atelier-lakeside-ligh")
            __printf "base16-atelier-lakeside-light" debug
            CYAN='^c#2d8f6f^'
            GREEN='^c#568c3b^'
            ORANGE='^c#935c25^'
            PINK='^c#b72dd2^'
            PURPLE='^c#6b6bb8^'
            RED='^c#d22d72^'
            YELLOW='^c#8a8a0f^'
            NORMAL='^c#1f292e^'
            ;;
        "doom-one")
            __printf "doom-one" debug
            CYAN='^c#5699af^'
            GREEN='^c#98be65^'
            ORANGE='^c#ff6c6b^'
            PINK='^c#c678dd^'
            PURPLE='^c#c678dd^'
            RED='^c#ff6c6b^'
            YELLOW='^c#da8548^'
            NORMAL='^c#dfdfdf^'
            ;;
        "dracula")
            __printf "dracula" debug
            CYAN='^c#8be9fd^'
            GREEN='^c#50fa7b^'
            ORANGE='^c#ffb86c^'
            PINK='^c#ff79c6^'
            PURPLE='^c#bd93f9^'
            RED='^c#ff5555^'
            YELLOW='^c#f1fa8c^'
            NORMAL='^c#f8f8f2^'
            ;;
        "everofest")
            __printf "everforest" debug
            ;;
        "gruvbox.dark")
            __printf "gruvbox.dark" debug
            #### Gruvbox Original Dark Hard
            # CYAN='^c#89b482^'
            # GREEN='^c#a9b665^'
            # ORANGE='^c#a9b665^'
            # PINK='^c#ea6962^'
            # PURPLE='^c#d3869b^'
            # RED='^c#ea6962^'
            # YELLOW='^c#e78a4e^'
            # NORMAL='^c#d4be98^'
            #### Gruvbox Base16 Gruvbox dark hard 256
            CYAN='^c#8ec07c^'
            GREEN='^c#b8bb26^'
            ORANGE='^c#a9b665^'
            PINK='^c#ea6962^'
            PURPLE='^c#d3869b^'
            RED='^c#fb4934^'
            YELLOW='^c#fabd2f^'
            NORMAL='^c#fbf1c7^'
            ;;
        "monokai")
            __printf "monokai" debug
            ;;
        "nord")
            __printf "nord" debug
            CYAN='^c#88c0d0^'
            GREEN='^c#a3be8c^'
            ORANGE='^c#bf616a^'
            PINK='^c#bf616a^'
            PURPLE='^c#b48ead^'
            RED='^c#bf616a^'
            YELLOW='^c#ebcb8b^'
            NORMAL='^c#e5e9f0^'
            ;;
        "papercolor.light")
            __printf "papercolor.light" debug
            CYAN='^c#3E999F^'
            GREEN='^c#718C00^'
            ORANGE='^c#d75f00^'
            PINK='^c#eeeeee^'
            PURPLE='^c#8700af^'
            RED='^c#d70000^'
            YELLOW='^c#d75f00^'
            NORMAL='^c#4D4D4C^'
            ;;
        "selenized.dark")
            __printf "selenized.dark" debug
            CYAN='^c#53d6c7^'
            GREEN='^c#84c747^'
            # ORANGE='^c#^'
            # PINK='^c#^'
            # PURPLE='^c#^'
            RED='^c#ff665c^'
            YELLOW='^c#ebc13d^'
            NORMAL='^c#adbcbc^'
            ;;
        "selenized.light")
            __printf "selenized.light" debug
            CYAN='^c#00978a^'
            GREEN='^c#428b00^'
            # ORANGE='^c#^'
            # PINK='^c#^'
            # PURPLE='^c#^'
            RED='^c#cc1729^'
            YELLOW='^c#a78300^'
            NORMAL='^c#53676d^'
            ;;
        "selenized.white")
            __printf "selenized.light" debug
            CYAN='^c#009a8a^'
            GREEN='^c#008400^'
            # ORANGE='^c#^'
            # PINK='^c#^'
            # PURPLE='^c#^'
            RED='^c#bf0000^'
            YELLOW='^c#af8500^'
            NORMAL='^c#474747^'
            ;;
        "solarized.light")
            __printf "solarized.light" debug
            CYAN='^c#2aa198^'
            GREEN='^c#859900^'
            ORANGE='^c#cb4b16^'
            PINK='^c#d33682^'
            PURPLE='^c#6c71c4^'
            RED='^c#dc322f^'
            YELLOW='^c#b58900^'
            NORMAL='^c#073642^'
            ;;
        "solarized.dark")
            __printf "solarized.dark" debug
            CYAN='^c#2aa198^'
            # GREEN='^c#859900^'
            GREEN='^c#51ef84^'
            ORANGE='^c#cb4b16^'
            PINK='^c#d33682^'
            PURPLE='^c#6c71c4^'
            # RED='^c#dc322f^'
            RED='^c#f5163b^'
            # YELLOW='^c#b58900^'
            YELLOW='^c#b27e28^'
            NORMAL='^c#eee8d5^'
            ;;
        "srcery")
            __printf "srcery" debug
            CYAN='^c#0aaeb3^'
            GREEN='^c#98BC37^'
            ORANGE='^c#ff5f00^'
            PINK='^c#f75341^'
            PURPLE='^c#e02c6d^'
            RED='^c#ef2f27^'
            YELLOW='^c#fbb829^'
            NORMAL='^c#fce8c3^'
            ;;
    esac
}

#### Function: xrdb parse
####
function __xrdb_parse()
{
      CYAN="^c$(xrdb -query -all | grep -E "^\*color6:|^\*\.color6:" | head -1 | awk '{ print $2 }')^"
     GREEN="^c$(xrdb -query -all | grep -E "^\*color2:|^\*\.color2:" | head -1 | awk '{ print $2 }')^"
    ORANGE="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"
      PINK="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"
    PURPLE="^c$(xrdb -query -all | grep -E "^\*color5:|^\*\.color5:" | head -1 | awk '{ print $2 }')^"
       RED="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"
    YELLOW="^c$(xrdb -query -all | grep -E "^\*color3:|^\*\.color3:" | head -1 | awk '{ print $2 }')^"
    NORMAL="^c$(xrdb -query -all | grep -E "^\*color0:|^\*\.color0:" | head -1 | awk '{ print $2 }')^"

    [[ -z ${CYAN} ]]   && CYAN='^c#00ffff^'
    [[ -z ${GREEN} ]]  && GREEN='^c#00d700^'
    [[ -z ${ORANGE} ]] && ORANGE='^c#d78700^'
    [[ -z ${PINK} ]]   && PINK='^c#d787af^'
    [[ -z ${PURPLE} ]] && PURPLE='^c#d700af^'
    [[ -z ${READ} ]]   && RED='^c#ff0000^'
    [[ -z ${YELLOW} ]] && YELLOW='^c#ffff00^'
    [[ -z ${NORMAL} ]] && NORMAL='^c#bbbbbb^'

    __printf "CYAN=${CYAN};GREEN=${GREEN};ORANGE=${ORANGE};PINK=${PINK};PURPLE=${PURPLE};RED=${RED};YELLOW=${YELLOW};NORMAL=${NORMAL}"
}

#### Function: load linux
####
function __load_linux()
{
    ncpu="$(cat /proc/cpuinfo | grep processor | wc -l)"
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_percent="$(echo "${load}/${ncpu}*100" | bc -l | sed -e "s/\..*//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "CPU: ${load}%"

    if [[ "${__sync_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] &&                                                       echo "${NORMAL} ${NORMAL}${GREEN} ${load}${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${load}${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] &&                                                       echo "${NORMAL} ${NORMAL}${RED}   ${load}${NORMAL}"
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] &&                                                       echo "${GREEN} ${load}${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "${YELLOW} ${load}${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] &&                                                       echo "${RED} ${load}${NORMAL}"
    fi
}

#### Function: load freebsd
####
function __load_freebsd()
{
    echo .
}

#### Function: temp linux
####
function __temp_linux()
{
    temp="$(acpi -t | head -1 | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | head -1 | sed -e "s/\..*//g")"

    if [[ "${__sync_flag-}" == "true" ]]
    then
        [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: -"
        [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL} ${NORMAL}${RED}-${NORMAL}"

        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] &&                                                   echo "${NORMAL} ${NORMAL}${GREEN} ${temp}${NORMAL}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${temp}${NORMAL}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] &&                                                   echo "${NORMAL} ${NORMAL}${RED}   ${temp}${NORMAL}"
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: -"
        [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${RED}  -${NORMAL}"

        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] &&                                                   echo "${GREEN} ${temp}${NORMAL}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "${YELLOW} ${temp}${NORMAL}"
        [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] &&                                                   echo "${RED} ${temp}${NORMAL}"
    fi
}

#### Function: temp freebsd
####
function __temp_freebsd()
{
    echo .
}

#### Function: memory linux
####
function __memory_linux()
{
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_available=$(cat /proc/meminfo | grep "MemAvailable:" | awk {' print $2'})
    mem_percent=$(echo "scale=2; ${mem_available}/${mem_total}*100" | bc -l | sed -e "s/\..*//g")

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "MEM: ${mem_percent}%"

    # [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <= 50" | bc -l) -eq 1 ]] &&                                                      echo "${NORMAL} ${NORMAL}${RED}   ${mem_percent}%${NORMAL}"
    # [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
    # [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] &&                                                      echo "${NORMAL} ${NORMAL}${GREEN} ${mem_percent}%${NORMAL}"

    if [[ "${__sync_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <   5" | bc -l) -eq 1 ]] &&                                                      echo "${NORMAL} ${NORMAL}${RED}   ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >=  5" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 10" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${RED}   ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 10" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 15" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 15" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 20" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 20" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 25" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 25" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 30" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 30" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 35" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 35" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 40" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 40" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 45" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 45" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 50" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 55" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 55" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 60" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 60" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 65" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 65" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 70" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 70" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 75" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 75" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 80" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 80" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 85" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 85" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 95" | bc -l) -eq 1 ]] && echo "${NORMAL} ${NORMAL}${GREEN} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 95" | bc -l) -eq 1 ]] &&                                                      echo "${NORMAL} ${NORMAL}${GREEN} ${mem_percent}%${NORMAL}"
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <   5" | bc -l) -eq 1 ]] &&                                                      echo "${RED} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >=  5" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 10" | bc -l) -eq 1 ]] && echo "${RED} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 10" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 15" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 15" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 20" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 20" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 25" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 25" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 30" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 30" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 35" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 35" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 40" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 40" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 45" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 45" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 50" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 55" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 55" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 60" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 60" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 65" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 65" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 70" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 70" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 75" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 75" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 80" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 80" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 85" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 85" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 95" | bc -l) -eq 1 ]] && echo "${GREEN} ${mem_percent}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 95" | bc -l) -eq 1 ]] &&                                                      echo "${GREEN} ${mem_percent}%${NORMAL}"
    fi
}

#### Function: memory freebsd
####
function __memory_freebsd()
{
    echo .
}

#### Function: battery linux
####
function __battery_linux()
{
    battery_enabled=$(find /sys/class/power_supply | grep -i bat | wc -l)

    if [[ "${__sync_flag-}" == "true" ]]
    then
        if [[ ${battery_enabled} -eq 0 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: -"
            [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL}  ${NORMAL}${RED}-${NORMAL}"
        else
            battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

            if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
            then
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: D ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${NORMAL}  ${NORMAL}${RED}   ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${NORMAL}  ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${NORMAL}  ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${NORMAL}  ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${NORMAL}  ${NORMAL}${GREEN} ${battery}%${NORMAL}"
            elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
            then
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: C ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${NORMAL} ${NORMAL}${RED}   ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${NORMAL} ${NORMAL}${YELLOW}${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${NORMAL} ${NORMAL}${GREEN} ${battery}%${NORMAL}"
            else
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: F ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL}  ${NORMAL}${GREEN}${battery}%${NORMAL}"
            fi
        fi
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        if [[ ${battery_enabled} -eq 0 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: -"
            [[ ${STATUSCOLOR} -eq 1 ]] && echo "${RED}  -${NORMAL}"
        else
            battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

            if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
            then
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: D ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${RED}  ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${YELLOW}  ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${YELLOW}  ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${YELLOW}  ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${GREEN}  ${battery}%${NORMAL}"
            elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
            then
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: C ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${RED} ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${YELLOW} ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${YELLOW} ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${YELLOW} ${battery}%${NORMAL}"
                [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${GREEN} ${battery}%${NORMAL}"
            else
                [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: F ${battery}%"
                [[ ${STATUSCOLOR} -eq 1 ]] && echo "${GREEN}  ${battery}%${NORMAL}"
            fi
        fi
    fi
}

#### Function: battery freebsd
####
function __battery_freebsd()
{
    echo .
}

#### Function: weather
####
function __weather()
{
    if [[ -e ${WTTR_FILE} ]]
    then
        file=$(find ${WTTR_FILE} -type f -cmin -${AGE})
        # echo "file='${file}'"

        if [[ "${file}" == "" ]]
        then
            # echo "file age not ok"
            __forecast
        else [[ "${file}" != "" ]]
            # echo "file age ok"
            [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "$(cat ${WTTR_FILE})"
            [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${CYAN}$(cat ${WTTR_FILE})${NORMAL}"
        fi
    else
        __forecast
    fi
}

#### Function: forecast
####
function __forecast()
{
    #### (1)
    # route=$(netstat -rn | grep "^0.0.0.0" | awk '{ print $2 }')
    #### (2)
    route=$(ip r | grep "^default" | awk '{ print $3 }')
    # echo "route='${route}'"

    ping -c1 -W1 ${route} &> /dev/null
    ping_status=${?}

    if [[ ${ping_status} -eq 0 ]]
    then
        # forecast="forecast"
        # forecast=$(curl -s http://wttr.in/Zagreb?format=1)
        # forecast=$(curl -s http://wttr.in/Zagreb?format=2)
        forecast=$(curl -s http://wttr.in/Zagreb?format='%l:+%c+%t+%h+%w+%m' > ${WTTR_FILE})
        echo -e "${forecast}"
    else
        echo ""
    fi
}

#### Function: network linux
####
function __network_linux()
{
    logfile=/dev/shm/netlog

    [ -f "${logfile}" ] || echo "0 0" > "${logfile}"
    read -r rxprev txprev < "${logfile}"

    rxcurrent="$(($(paste -d '+' /sys/class/net/[ew]*/statistics/rx_bytes)))"
    txcurrent="$(($(paste -d '+' /sys/class/net/[ew]*/statistics/tx_bytes)))"

    rx=$(echo "scale=2; (${rxcurrent}-${rxprev})" | bc)
    tx=$(echo "scale=2; (${txcurrent}-${txprev})" | bc)

    if [[ "${__sync_flag-}" == "true" ]]
    then
        if [[ ${rx} -gt 1048576 ]]
        then
            rx=$(echo "scale=2; (${rx} / 1024/1024)" | bc | awk '{printf("%04d", $1)}')
            rx="${RED}${rx}${NORMAL} MB/s"
        elif [[ ${rx} -gt 1024 ]]
        then
            rx=$(echo "scale=2; (${rx} / 1024)" | bc | awk '{printf("%04d", $1)}')
            rx="${YELLOW}${rx}${NORMAL} KB/s"
        else
            rx=$(echo "scale=2; (${rx} / 1)" | bc | awk '{printf("%04d", $1)}')
            rx="${GREEN}${rx}${NORMAL}  B/s"
        fi

        if [[ ${tx} -gt 1048576 ]]
        then
            tx=$(echo "scale=2; (${tx} / 1024/1024)" | bc | awk '{printf("%04d", $1)}')
            tx="${RED}${tx}${NORMAL} MB/s"
        elif [[ ${tx} -gt 1024 ]]
        then
            tx=$(echo "scale=2; (${tx} / 1024)" | bc | awk '{printf("%04d", $1)}')
            tx="${YELLOW}${tx}${NORMAL} KB/s"
        else
            tx=$(echo "scale=2; (${tx} / 1)" | bc | awk '{printf("%04d", $1)}')
            tx="${GREEN}${tx}${NORMAL}  B/s"
        fi

        echo "${rxcurrent} ${txcurrent}" > "${logfile}"

        echo -e "${NORMAL} ${rx}  ${tx}${NORMAL} "
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        if [[ ${rx} -gt 1048576 ]]
        then
            rx=$(echo "scale=2; (${rx} / 1024/1024)" | bc | awk '{printf("%04d", $1)}')
            rx="${rx} MB/s"
        elif [[ ${rx} -gt 1024 ]]
        then
            rx=$(echo "scale=2; (${rx} / 1024)" | bc | awk '{printf("%04d", $1)}')
            rx="${rx} KB/s"
        else
            rx=$(echo "scale=2; (${rx} / 1)" | bc | awk '{printf("%04d", $1)}')
            rx="${rx}  B/s"
        fi

        if [[ ${tx} -gt 1048576 ]]
        then
            tx=$(echo "scale=2; (${tx} / 1024/1024)" | bc | awk '{printf("%04d", $1)}')
            tx="${tx} MB/s"
        elif [[ ${tx} -gt 1024 ]]
        then
            tx=$(echo "scale=2; (${tx} / 1024)" | bc | awk '{printf("%04d", $1)}')
            tx="${tx} KB/s"
        else
            tx=$(echo "scale=2; (${tx} / 1)" | bc | awk '{printf("%04d", $1)}')
            tx="${tx}  B/s"
        fi

        echo "${rxcurrent} ${txcurrent}" > "${logfile}"

        echo -e " ${rx}  ${tx} "
    fi
}

#### Function: network freebsd
####
function __network_freebsd()
{
    echo .
}

#### Function: time
####
function __time()
{
    date="$(date +"%a %Y-%m-%d")"
    time="$(date +"%H:%M:%S")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "${date} ${time}"

    if [[ "${__sync_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${CYAN} ${NORMAL}${date}${NORMAL} ${CYAN} ${NORMAL}${time}${NORMAL}"
    fi

    if [[ "${__async_flag-}" == "true" ]]
    then
        [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${CYAN} ${date}  ${time}"
    fi
}

#### Function: spaces
####
function __spaces()
{
    space=""
    for (( i=1; i<=${DEFAULT_SPACES}; i++))
    do
        space="${space} "
    done
    echo "${space}"
}

#### Function: dwm_log
####
function __dwm_logo()
{
    echo .
}

#### Function: async_process
####
function __async_process()
{
    #### arg
    async="${1}"
    __printf "async='${async}'" debug

    case ${async} in
        "sb-load")
            [[ "${OS}" = "Linux"   ]] && __load_linux
            [[ "${OS}" = "FreeBSD" ]] && __load_freebsd
            ;;
        "sb-temp")
            [[ "${OS}" = "Linux"   ]] && __temp_linux
            [[ "${OS}" = "FreeBSD" ]] && __temp_freebsd
            ;;
        "sb-memory")
            [[ "${OS}" = "Linux"   ]] && __memory_linux
            [[ "${OS}" = "FreeBSD" ]] && __memory_freebsd
            ;;
        "sb-battery")
            [[ "${OS}" = "Linux"   ]] && __battery_linux
            [[ "${OS}" = "FreeBSD" ]] && __battery_freebsd
            ;;
        "sb-weather")
            # __weather

            echo .
            ;;
        "sb-forecast")
            # __forecast

            echo .
            ;;
        "sb-network")
            [[ "${OS}" = "Linux"   ]] && __network_linux
            [[ "${OS}" = "FreeBSD" ]] && __network_freebsd
            ;;
        "sb-time")
            __time
            ;;
    esac
}

#### Function: sync_process
####
function __sync_process()
{
    #### check status
    IAM=(`pgrep -d " " -f ${0//*\//}`)
    [ ${#IAM[@]} -gt 1 ] && { echo I AM running; exit 127; } || echo "I AM not running. Running now ;-p"

    #### Add an artificial sleep to wait for the IPC handler to be ready to process requests
    sleep 0.2

    #### draw status
    if [[ "${OS}" = "Linux" ]]
    then
        while true;
        do
            #### xrdb_parse
            # __xrdb_parse

            #### xsetroot
            if [[ ${STATUSCOLOR} -eq 0 ]]
            then
                #### no-clickable statusbar
                # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__weather) | $(__network_linux) | $(__time) ]"
                # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__time) ]"
                # xsetroot -name "[ $(__load_linux) | $(__memory_linux) | $(__battery_linux) | $(__network_linux) | $(__time) ]"

                #### clickable statusbar
                xsetroot -name "[$(printf '\x04 %s' $(__load_linux)) |$(printf '\x04 %s' $(__memory_linux)) | $(__battery_linux) | $(__network_linux) |$(printf '\x06 %s' $(__time)) ]"
            elif [[ ${STATUSCOLOR} -eq 1 ]]
            then
                #### no-clickable statusbar
                # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__weather) | $(__network_linux) | $(__time) ]"
                # xsetroot -name "${NORMAL}[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__network_linux) | $(__time) ]${NORMAL}"
                # xsetroot -name "${NORMAL}[ $(__load_linux) | $(__memory_linux) | $(__battery_linux) | $(__network_linux) | $(__time) ]${NORMAL}"

                #### clickable statusbar
                xsetroot -name "${NORMAL}[$(printf '\x04 %s' $(__load_linux)) |$(printf '\x04 %s' $(__memory_linux)) | $(__battery_linux) | $(__network_linux) |$(printf '\x06 %s' $(__time)) ]${NORMAL}"
            fi

            #### sleep
            sleep 1
        done
    elif [[ "${OS}" = "FreeBSD" ]]
    then
        while true;
        do
            #### xrdb_parse
            # __xrdb_parse

            #### xsetroot
            if [[ ${STATUSCOLOR} -eq 0 ]]
            then
                #### no-clickable statusbar
                # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | # $(__weather) | $(__network_freebsd) | $(__time) ]$(__spaces)"
                # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]"
                xsetroot -name "[ $(__load_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]"
            elif [[ ${STATUSCOLOR} -eq 1 ]]
            then
                #### no-clickable statusbar
                # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | # # $(__weather_freebsd) | $(__network_freebsd) | $(__time) ] $(__spaces)"
                xsetroot -name "${NORMAL}[ $(__load_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]${NORMAL}"
            fi

            #### sleep
            sleep 1
        done
    fi
}


###############################################################################
## main
###############################################################################

#### Banner
# __banner

#### Command Line
__theme_flag=""
__async_flag=""
__sync_flag=""

while getopts "ha:st:l:" opt;
do
    case ${opt} in
        h)
            __help
            exit ${EXIT_OK}
            ;;
        a)
            __async_flag="true"
            ASYNC=${OPTARG}
            ;;
        s)
            __sync_flag="true"
            ;;
        t)
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

if [[ -z "${__theme_flag}" ]]
then
    __printf "Missing theme arguments!" error
    __help
    exit ${EXIT_ERROR}
fi

if [[ "${__async_flag-}" == "true" ]]
then
    #### Theme
    __theme ${THEME}

    #### async process
    __async_process ${ASYNC}
fi

if [[ "${__sync_flag-}" == "true" ]]
then
    #### Theme
    __theme ${THEME}

    #### sync process
    __sync_process
fi

#### Done
__printf "done!" debug

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
