#!/usr/bin/env bash
#===============================================================================
#
#          FILE: dwm.status.sh
#
#         USAGE: ./dwm.status.sh [ -h | -s <theme> ]
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2021-11-06 14:14:32
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

#### ansi - default
CYAN='^c#00ffff^'
GREEN='^c#00d700^'
ORANGE='^c#d78700^'
PINK='^c#d787af^'
PURPLE='^c#d700af^'
RED='^c#ff0000^'
YELLOW='^c#ffff00^'
NORMAL='^c#bbbbbb^'


###############################################################################
## functions
###############################################################################

#### Function: Printf
####
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
    __printf "Usage: ${0} [ -h | -s <theme> ]"
    __printf "  -h                Help"
    __printf "  -s <theme>        Theme"
    __printf "  -l <file>  Log to <file>"
    __printf "Examples:"
    __printf "   ${0} -s ansi"
    __printf "   ${0} -s base16-atelier-lakeside-light"
    __printf "   ${0} -s base16-google-light"
    __printf "   ${0} -s base16-gruvbox-dark-soft"
    __printf "   ${0} -s dracula"
    __printf "   ${0} -s everforest"
    __printf "   ${0} -s gruvbox"
    __printf "   ${0} -s monokai"
    __printf "   ${0} -s nord"
    __printf "   ${0} -s papercolor.light"
    __printf "   ${0} -s solarized.dark"
    __printf "   ${0} -s solarized.light"
    __printf "   ${0} -s srcery"
}

#### Function: Theme environment
####
function __theme()
{
    #### arg
    theme="${1}"
    __printf "theme='${theme}'" debug

    #### ansi
    case ${theme} in
        "ansi")
            __printf "ansi"
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
            __printf "base16-atelier-lakeside-light"
            CYAN='^c#2d8f6f^'
            GREEN='^c#568c3b^'
            ORANGE='^c#935c25^'
            PINK='^c#b72dd2^'
            PURPLE='^c#6b6bb8^'
            RED='^c#d22d72^'
            YELLOW='^c#8a8a0f^'
            NORMAL='^c#1f292e^'
            ;;
        "dracula")
            __printf "dracula"
            CYAN='^c#8be9fd^'
            GREEN='^c#50fa7b^'
            ORANGE='^c#ffb86c^'
            PINK='^c#ff79c6^'
            PURPLE='^c#bd93f9^'
            RED='^c#ff5555^'
            YELLOW='^c#f1fa8c^'
            NORMAL='^c#f8f8f2^'
            ;;
        "gruvbox")
            __printf "gruvbox"
            CYAN='^c#89b482^'
            GREEN='^c#a9b665^'
            ORANGE='^c#a9b665^'
            PINK='^c#ea6962^'
            PURPLE='^c#d3869b^'
            RED='^c#ea6962^'
            YELLOW='^c#e78a4e^'
            NORMAL='^c#d4be98^'
            ;;
        "solarized.light")
            __printf "solarized light"
            CYAN='^c#2aa198^'
            GREEN='^c#859900^'
            ORANGE='^c#cb4b16^'
            PINK='^c#d33682^'
            PURPLE='^c#6c71c4^'
            RED='^c#dc322f^'
            YELLOW='^c#b58900^'
            NORMAL='^c#073642^'
            ;;
        "solarizded.dark")
            __printf "solarized dark"
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

#### Function: load
####
function __load_linux()
{
    ncpu="$(cat /proc/cpuinfo | grep processor | wc -l)"
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_percent="$(echo "${load}/${ncpu}*100" | bc -l | sed -e "s/\..*//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "CPU: ${load}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] && echo "${NORMAL}: ${GREEN}${load}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${YELLOW}${load}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${RED}${load}${NORMAL}"
}

function __load_freebsd()
{
    echo .
}

#### Function: temp
####
function __temp_linux()
{
    temp="$(acpi -t | head -1 | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | head -1 | sed -e "s/\..*//g")"

    [[ -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: -"
    [[ -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL}  ${RED}-${NORMAL}"

    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${GREEN}${temp}${NORMAL}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "${NORMAL} ${YELLOW}${temp}${NORMAL}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${RED}${temp}${NORMAL}"
}

function __temp_freebsd()
{
    echo .
}

#### Function: memory
####
function __memory_linux()
{
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_available=$(cat /proc/meminfo | grep "MemAvailable:" | awk {' print $2'})
    mem_percent=$(echo "scale=2; ${mem_available}/${mem_total}*100" | bc -l | sed -e "s/\..*//g")

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "MEM: ${mem_percent}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <= 50" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${RED}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${YELLOW}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && echo "${NORMAL}  ${GREEN}${mem_percent}%${NORMAL}"
}

function __memory_freebsd()
{
    echo .
}

#### Function:  battery
####
function __battery_linux()
{
    battery_enabled=$(find /sys/class/power_supply | grep -i bat | wc -l)

    if [[ ${battery_enabled} -eq 0 ]]
    then
        [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: -"
        [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL}  ${RED}-${NORMAL}"
    else
        if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="D"
            [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${RED}D${NORMAL}"
        elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="C"
            [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${RED}C${NORMAL}"
        else
            [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="F"
            [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${RED}F${NORMAL}"
        fi
        battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

        [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: ${battery_status}${battery}%"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 25 ]] && echo "${NORMAL} ${battery_status} ${RED}${battery}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 25 ]] && [[ ${battery} -lt 80 ]] && echo "${NORMAL} ${battery_status} ${YELLOW}${battery}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -ge 80 ]] && echo "${NORMAL}  ${battery_status} ${GREEN}${battery}%${NORMAL}"
    fi
}

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

#### Function: network
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

    # echo "$(bc <<< "scale=2; (${rxcurrent}-${rxprev}) / 10^6")" "$(bc <<< "scale=2; (${txcurrent}-${txprev}) / 10^6")"
    echo -e "${NORMAL}⬇️ ${rx} ⬆️ ${tx} ${NORMAL}"
}

function __network_freebsd()
{
    echo .
}

#### Function: time
####
function __time()
{
    date="$(date +"%a %Y-%m-%d %H:%M:%S")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "${date}"
    # [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${ORANGE}${date}${NORMAL}"
    # [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${CYAN}${date}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "  ${NORMAL}${date}${NORMAL}"
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

#### Function: process
####
function __process()
{
    dwm_status_detect=$(ps -ef | grep -v "grep" | grep "dwm.status.sh" | wc -l | awk '{$1=$1};1')
    echo "dwm_status_detect='${dwm_status_detect}'"

    if [[ "${OS}" = "Linux" ]]
    then
        if [[ ${dwm_status_detect} -eq 2 ]]
        # if [[ ${dwm_status_detect} -eq 3 ]]
        then
            while true;
            do
                #### xsetroot
                if [[ ${STATUSCOLOR} -eq 0 ]]
                then
                    # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__weather) | $(__network_linux) | $(__time) ]"
                    # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__time) ]"
                    xsetroot -name "[ $(__load_linux) | $(__memory_linux) | $(__battery_linux) | $(__time) ]"
                elif [[ ${STATUSCOLOR} -eq 1 ]]
                then
                    # xsetroot -name "[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__weather) | $(__network_linux) | $(__time) ]"
                    xsetroot -name "${NORMAL}[ $(__load_linux) | $(__temp_linux) | $(__memory_linux) | $(__battery_linux) | $(__network_linux) | $(__time) ]${NORMAL}"
                fi
                sleep 1
            done
        else
            echo "dwm.status.sh is running"
        fi
    elif [[ "${OS}" = "FreeBSD" ]]
    then
        if [[ ${dwm_status_detect} -eq 0 ]]
        then
            while true;
            do
                #### xsetroot
                if [[ ${STATUSCOLOR} -eq 0 ]]
                then
                    # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | # $(__weather) | $(__network_freebsd) | $(__time) ]$(__spaces)"
                    # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]"
                    xsetroot -name "[ $(__load_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]"
                elif [[ ${STATUSCOLOR} -eq 1 ]]
                then
                    # xsetroot -name "[ $(__load_freebsd) | $(__temp_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | # # $(__weather_freebsd) | $(__network_freebsd) | $(__time) ] $(__spaces)"
                    xsetroot -name "${NORMAL}[ $(__load_freebsd) | $(__memory_freebsd) | $(__battery_freebsd) | $(__time) ]${NORMAL}"
                fi
                sleep 1
            done
        else
            echo "dwm.status.sh is running"
        fi
    fi
}


###############################################################################
## main
###############################################################################

#### Banner
__banner

#### Command Line
__theme_flag=""

while getopts "hs:l:" opt;
do
    case ${opt} in
        h)
            __help
            exit ${EXIT_OK}
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

if [[ -z "${__theme_flag}" ]]
then
    __printf "Missing arguments!" error
    __help
    exit ${EXIT_ERROR}
else
    if [[ "${__theme_flag-}" == "true" ]]
    then
        #### Theme
        __theme ${THEME}
        #### keyboard
        __process
    fi
fi

#### Done
__printf "done!"

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
