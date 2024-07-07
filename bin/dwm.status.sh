#!/usr/bin/env bash
#===============================================================================
#
#          FILE: dwm.status.sh
#
#         USAGE: ./dwm.status.sh [ -h | -a <dwmblock> | -s | -f <function> | -b <button> ]]
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
    __printf "Usage: ${0} [ -h | -a <dwmblock> | -s | -f <function> | -b <button> ]"
    __printf "  -h                Help"
    __printf "  -a <dwmblock>     DWM Status - async"
    __printf "  -s                DWM Status - sync"
    __printf "  -f                Function set"
    __printf "  -l <file>         Log to <file>"
    __printf ""
    __printf "Functions:"
    __printf "   ${0} -f volume-set -b 1"
    __printf "   ${0} -f mic-set -b 1"
    __printf ""
    __printf "Examples:"
    __printf "${0} -f volume-set -b 1"
    __printf "${0} -f mic-set -b 1"
    __printf "${0} -a sb-temp"
    __printf "${0} -a sb-load"
    __printf "${0} -a sb-memory"
    __printf "${0} -a sb-battery"
    __printf "${0} -a sb-network"
    __printf "${0} -a sb-keymap"
    __printf "${0} -a sb-volume"
    __printf "${0} -a sb-mic"
    __printf "${0} -a sb-time"
    __printf "${0} -s"
}

#### Function: xrdb parse
####
function __xrdb_parse()
{
    NORMAL="^c$(xrdb -query -all | grep -E "^\*color0:|^\*\.color0:" | head -1 | awk '{ print $2 }')^"
       RED="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"
     GREEN="^c$(xrdb -query -all | grep -E "^\*color2:|^\*\.color2:" | head -1 | awk '{ print $2 }')^"
    YELLOW="^c$(xrdb -query -all | grep -E "^\*color3:|^\*\.color3:" | head -1 | awk '{ print $2 }')^"
      BLUE="^c$(xrdb -query -all | grep -E "^\*color4:|^\*\.color4:" | head -1 | awk '{ print $2 }')^"
    PURPLE="^c$(xrdb -query -all | grep -E "^\*color5:|^\*\.color5:" | head -1 | awk '{ print $2 }')^"
      CYAN="^c$(xrdb -query -all | grep -E "^\*color6:|^\*\.color6:" | head -1 | awk '{ print $2 }')^"
     WHITE="^c$(xrdb -query -all | grep -E "^\*color7:|^\*\.color7:" | head -1 | awk '{ print $2 }')^"

    ORANGE="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"
      PINK="^c$(xrdb -query -all | grep -E "^\*color1:|^\*\.color1:" | head -1 | awk '{ print $2 }')^"

    [[ -z ${NORMAL} ]] && NORMAL='^c#bbbbbb^'
    [[ -z ${READ} ]]   && RED='^c#ff0000^'
    [[ -z ${GREEN} ]]  && GREEN='^c#00d700^'
    [[ -z ${YELLOW} ]] && YELLOW='^c#ffff00^'
    [[ -z ${BLUE} ]]   && BLUE='^c#ffff00^'
    [[ -z ${PURPLE} ]] && PURPLE='^c#d700af^'
    [[ -z ${CYAN} ]]   && CYAN='^c#00ffff^'
    [[ -z ${WHITE} ]]  && WHITE='^c#00ffff^'

    [[ -z ${ORANGE} ]] && ORANGE='^c#d78700^'
    [[ -z ${PINK} ]]   && PINK='^c#d787af^'

    # __printf "NORMAL=${NORMAL};RED=${RED};GREEN=${GREEN};YELLOW=${YELLOW};BLUE=${BLUE};PURPLE=${PURPLE};CYAN=${CYAN};WHITE=${WHITE};ORANGE=${ORANGE};PINK=${PINK}"
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

#### Function: temp linux
####
function __temp_linux()
{
    temp="$(acpi -t | head -1 | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | head -1 | sed -e "s/\..*//g")"

    [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: -"
    [[ -z ${temp} ]]   && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${READ} -${WHITE}"

    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] &&                                                   echo "${GREEN} ${temp}${WHITE}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "${YELLOW} ${temp}${WHITE}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] &&                                                   echo "${RED} ${temp}${WHITE}"
}

#### Function: temp freebsd
####
function __temp_freebsd()
{
    echo .
}

#### Function: load linux
####
function __load_linux()
{
    ncpu="$(cat /proc/cpuinfo | grep processor | wc -l)"
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_percent="$(echo "${load}/${ncpu}*100" | bc -l | sed -e "s/\..*//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "CPU: ${load}%"

    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] &&                                                       echo "${GREEN} ${load}${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "${YELLOW} ${load}${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] &&                                                       echo "${RED} ${load}${WHITE}"
}

#### Function: load freebsd
####
function __load_freebsd()
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

    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <   5" | bc -l) -eq 1 ]] &&                                                      echo "${RED} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >=  5" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 10" | bc -l) -eq 1 ]] && echo "${RED} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 10" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 15" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 15" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 20" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 20" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 25" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 25" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 30" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 30" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 35" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 35" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 40" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 40" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 45" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 45" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 50" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 55" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 55" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 60" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 60" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 65" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 65" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 70" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 70" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 75" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 75" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 80" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 80" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 85" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 85" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${YELLOW} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 95" | bc -l) -eq 1 ]] && echo "${GREEN} ${mem_percent}%${WHITE}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 95" | bc -l) -eq 1 ]] &&                                                      echo "${GREEN} ${mem_percent}%${WHITE}"
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

    if [[ ${battery_enabled} -eq 0 ]]
    then
        [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: -"
        [[ ${STATUSCOLOR} -eq 1 ]] && echo "${RED} -${WHITE}"
    else
        battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

        if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: D ${battery}%"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${RED}  ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${YELLOW}  ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${YELLOW}  ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${YELLOW}  ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${GREEN}  ${battery}%${WHITE}"
        elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
        then
            [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: C ${battery}%"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 20 ]] &&                            echo "${RED} ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 20 ]] && [[ ${battery} -le 40 ]] && echo "${YELLOW} ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 40 ]] && [[ ${battery} -le 60 ]] && echo "${YELLOW} ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 60 ]] && [[ ${battery} -le 80 ]] && echo "${YELLOW} ${battery}%${WHITE}"
            [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] &&                            echo "${GREEN} ${battery}%${WHITE}"
        else
            [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: F ${battery}%"
            [[ ${STATUSCOLOR} -eq 1 ]] && echo "${GREEN}  ${battery}%${WHITE}"
        fi
    fi
}

#### Function: battery freebsd
####
function __battery_freebsd()
{
    echo .
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

    if [[ ${rx} -gt 1048576 ]]
    then
        rx=$(echo "scale=2; (${rx} / 1024/1024)" | bc | awk '{printf("%03d", $1)}')
        rx="${RED}  ${rx} MB/s"
    elif [[ ${rx} -gt 1024 ]]
    then
        rx=$(echo "scale=2; (${rx} / 1024)" | bc | awk '{printf("%03d", $1)}')
        rx="${YELLOW} ${rx} KB/s"
    else
        rx=$(echo "scale=2; (${rx} / 1)" | bc | awk '{printf("%03d", $1)}')
        rx="${GREEN} ${rx}  B/s"
    fi

    if [[ ${tx} -gt 1048576 ]]
    then
        tx=$(echo "scale=2; (${tx} / 1024/1024)" | bc | awk '{printf("%03d", $1)}')
        tx="${RED} ${tx} MB/s"
    elif [[ ${tx} -gt 1024 ]]
    then
        tx=$(echo "scale=2; (${tx} / 1024)" | bc | awk '{printf("%03d", $1)}')
        tx="${YELLOW} ${tx} KB/s"
    else
        tx=$(echo "scale=2; (${tx} / 1)" | bc | awk '{printf("%03d", $1)}')
        tx="${GREEN} ${tx}  B/s"
    fi

    echo "${rxcurrent} ${txcurrent}" > "${logfile}"

    echo -e "${rx} ${tx}"
}

#### Function: network freebsd
####
function __network_freebsd()
{
    echo .
}

#### Function: xkb_keymap
####
function __xkb_keymap()
{
    xkb_symboks=$(setxkbmap -print | grep xkb_symbols | awk '{print $4}' | awk -F"+" '{print $2}')

    [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "${xkb_symboks}"
    [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${WHITE}⌨ ${xkb_symboks}${WHITE}"
}

#### Function: volume
####
function __volume()
{
    volume=$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')
    volume_mute=$(amixer get Master | grep -E "Front Left:|Front Right:" | grep "\[off\]" | wc -l)

    [[ -z ${volume} ]]   && [[ ${STATUSCOLOR} -eq 0 ]] && echo "VOL: -"
    [[ -z ${volume} ]]   && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${RED}󰖀 -${NORMAL}"

    [[ ! -z ${volume} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${volume_mute} == 2" | bc -l) -eq 1 ]] &&                                                                                                   echo -e "${RED}󰖁 ${volume}%${WHITE}"
    [[ ! -z ${volume} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${volume_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${volume} ==  0" | bc -l) -eq 1 ]] &&                                                  echo -e "${RED}󰖁 ${volume}%${WHITE}"
    [[ ! -z ${volume} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${volume_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${volume} >   0" | bc -l) -eq 1 ]] && [[ $(echo "${volume} <= 40" | bc -l) -eq 1 ]] && echo -e "${GREEN}󰕿 ${volume}%${WHITE}"
    [[ ! -z ${volume} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${volume_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${volume} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${volume} <  80" | bc -l) -eq 1 ]] && echo -e "${YELLOW}󰖀 ${volume}%${WHITE}"
    [[ ! -z ${volume} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${volume_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${volume} >= 80" | bc -l) -eq 1 ]] &&                                                  echo -e "${RED}󰕾 ${volume}%${WHITE}"
}

#### Function: volume set
####
function __volume_set()
{
    echo "button=${BUTTON}" debug

    case ${BUTTON} in
        1)
            #### alsa
            notify-send "amixer sset Master 1+ 10%+"
            amixer sset Master 1+ 10%+
            #### pulse secure
            # /usr/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%
            #### pipewire
            # /usr/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+
            ;;
        2)
            #### alsa
            notify-send "amixer sset Master 1+ toggle"
            amixer sset Master 1+ toggle
            #### pulse secure
            # /usr/bin/pactl set-sink-mute   @DEFAULT_SINK@ toggle
            #### pipewire
            # /usr/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
            ;;
        3)
            #### alsa
            notify-send "amixer sset Master 1+ 10%-"
            amixer sset Master 1+ 10%-
            #### pulse secure
            # /usr/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%
            #### pipewire
            # /usr/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
            ;;
    esac
}

#### Function: mic
####
function __mic()
{
    mic=$(amixer get Capture | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')
    mic_mute=$(amixer get Capture | grep -E "Front Left:|Front Right:" | grep "\[off\]" | wc -l)

    [[ -z ${mic} ]]   && [[ ${STATUSCOLOR} -eq 0 ]] && echo "MIC: -"
    [[ -z ${mic} ]]   && [[ ${STATUSCOLOR} -eq 1 ]] && echo "${RED}󰍬 -${NORMAL}"

    [[ ! -z ${mic} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mic_mute} == 2" | bc -l) -eq 1 ]] &&                                               echo -e "${RED} ${mic}%${NORMAL}"
    [[ ! -z ${mic} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mic_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${mic} ==  0" | bc -l) -eq 1 ]] && echo -e "${RED} ${mic}%${NORMAL}"
    [[ ! -z ${mic} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mic_mute} != 2" | bc -l) -eq 1 ]] && [[ $(echo "${mic} >   0" | bc -l) -eq 1 ]] && echo -e "${GREEN}󰍬 ${mic}%${NORMAL}"
}

#### Function: mic set
####
function __mic_set()
{
    echo "button=${BUTTON}" debug

    case ${BUTTON} in
        1)
            #### alsa
            notify-send "amixer sset Capture 1+ 10%+"
            amixer sset Capture 1+ 10%+
            ;;
        2)
            #### alsa
            notify-send "amixer sset Capture 1+ toggle"
            amixer sset Capture 1+ toggle
            ;;
        3)
            #### alsa
            notify-send "amixer sset Capture 1+ 10%-"
            amixer sset Capture 1+ 10%-
            ;;
    esac
}

#### Function: time
####
function __time()
{
    date="$(date +"%a %Y-%m-%d")"
    time="$(date +"%H:%M:%S")"
    hour="$(date +"%l" | sed -e "s/ //g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "${date} ${time}"

    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 1"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑋 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 2"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑌 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 3"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑍 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 4"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑎 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 5"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑏 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 6"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑐 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 7"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑑 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 8"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑒 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 9"  | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑓 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 10" | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑔 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 11" | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑕 ${time}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${hour} == 12" | bc -l) -eq 1 ]] && echo -e "${WHITE} ${date} 󱑖 ${time}"
}

#### Function: async_process
####
function __async_process()
{
    #### arg
    async="${1}"
    __printf "async='${async}'" debug

    #### xrdb_parse
    __xrdb_parse

    case ${async} in
        "sb-temp")
            [[ "${OS}" = "Linux"   ]] && __temp_linux
            [[ "${OS}" = "FreeBSD" ]] && __temp_freebsd
            ;;
        "sb-load")
            [[ "${OS}" = "Linux"   ]] && printf '\x04 %s' && __load_linux
            [[ "${OS}" = "FreeBSD" ]] && __load_freebsd
            ;;
        "sb-memory")
            [[ "${OS}" = "Linux"   ]] && printf '\x04 %s' && __memory_linux
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
            printf '\x06 %s';
            __time
            ;;
        "sb-keymap")
            __xkb_keymap
            ;;
        "sb-volume")
            printf '\x07 %s' && __volume
            ;;
        "sb-mic")
            printf '\x08 %s' && __mic
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
            __xrdb_parse

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
            __xrdb_parse

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

#### Function: function_process
####
function __function_process()
{
    #### arg
    function="${1}"
    __printf "function='${function}'" debug

    case ${function} in
        "volume-set")
             __volume_set
            ;;
        "mic-set")
            __mic_set
            ;;
    esac
}


###############################################################################
## main
###############################################################################

#### Banner
# __banner

#### Command Line
__async_flag=""
__sync_flag=""
__function_flag=""
__button_flag=""

while getopts "ha:sf:b:l:" opt;
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
        f)
            __function_flag="true"
            __theme_flag="true"
            FUNCTION=${OPTARG}
            ;;
        b)
            __button_flag="true"
            BUTTON=${OPTARG}
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

if [[ -z "${__async_flag-}" ]] && [[ -z "${__sync_flag-}" ]] && [[ -z "${__function_flag-}" ]]
then
    __help
    exit ${EXIT_ERROR}
fi

if [[ "${__async_flag-}" == "true" ]]
then
    #### async process
    __async_process ${ASYNC}
fi

if [[ "${__sync_flag-}" == "true" ]]
then
    #### sync process
    __sync_process
fi

if [[ "${__function_flag-}" == "true" ]]
then
    if [[ -z "${__button_flag}" ]]
    then
        __printf "Missing button arguments!" error
        __help
        exit ${EXIT_ERROR}
    fi

    #### Function
    __function_process ${FUNCTION}
fi

#### Done
__printf "done!" debug

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
