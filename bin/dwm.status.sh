#!/bin/bash
export PATH=$HOME/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

DEFAULT_SPACES=7
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"
STATUSCOLOR=1

# RED='^c#ff0000^'
# GREEN='^c#00ff00^'
# YELLOW='^c#ffff00^'
# CYAN='^c#00ffff^'
# NORMAL='^c#bbbbbb^'

CYAN='^c#8be9fd^'
GREEN='^c#50fa7b^'
ORANGE='^c#ffb86c^'
PINK='^c#ff79c6^'
PURPLE='^c#bd93f9^'
RED='^c#ff5555^'
YELLOW='^c#f1fa8c^'
NORMAL='^c#f8f8f2^'

GLYPH_BATTERY="^r0,7,2,4^^r2,4,22,10^^c#000000^^r3,5,20,8^^c#ffffff^^r10,5,13,8^^d^^f24^"

#### load
function __load()
{
    ncpu="$(cat /proc/cpuinfo | grep processor | wc -l)"
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_percent="$(echo "${load}/${ncpu}*100" | bc -l | sed -e "s/\..*//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "CPU: ${load}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] && echo "${NORMAL}CPU: ${GREEN}${load}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "${NORMAL}CPU: ${YELLOW}${load}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] && echo "${NORMAL}CPU: ${RED}${load}${NORMAL}"
}

#### temp
function __temp()
{
    temp="$(acpi -t | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | sed -e "s/\..*//g")"

    [[ -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: -"
    [[ -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && echo "Temp: ${RED}-${NORMAL}"

    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] && echo "${NORMAL}Temp: ${GREEN}${temp}${NORMAL}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "${NORMAL}Temp: ${YELLOW}${temp}${NORMAL}"
    [[ ! -z ${temp} ]] && [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] && echo "${NORMAL}Temp: ${RED}${temp}${NORMAL}"
}

#### memory
function __memory()
{
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_available=$(cat /proc/meminfo | grep "MemAvailable:" | awk {' print $2'})
    mem_percent=$(echo "scale=2; ${mem_available}/${mem_total}*100" | bc -l | sed -e "s/\..*//g")

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "MEM: ${mem_percent}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} <= 50" | bc -l) -eq 1 ]] && echo "${NORMAL}MEM: ${RED}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "${NORMAL}MEM: ${YELLOW}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && echo "${NORMAL}MEM: ${GREEN}${mem_percent}%${NORMAL}"
}

#### battery
function __battery()
{
    battery_enabled=$(find /sys/class/power_supply | grep -i bat | wc -l)

    if [[ ${battery_enabled} -eq 0 ]]
    then
        [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: -"
        [[ ${STATUSCOLOR} -eq 1 ]] && echo "${NORMAL}BAT: ${RED}-${NORMAL}"
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
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -le 25 ]] && echo "${NORMAL}BAT: ${battery_status} ${RED}${battery}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 25 ]] && [[ ${battery} -lt 80 ]] && echo "${NORMAL}BAT: ${battery_status} ${YELLOW}${battery}%${NORMAL}"
        [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -ge 80 ]] && echo "${NORMAL}BAT: ${battery_status} ${GREEN}${battery}%${NORMAL}"
    fi
}

#### weather
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

#### network
function __network()
{
    #### (1)
    ip_private=$(hostname -I | awk {'print $1'})
    ip_public=$(curl -s https://ipinfo.io/ip)
    iface=$(ip -o addr show up primary scope global | grep $(hostname -I | awk {'print $1'}) | awk '{ print $2 }')

    echo -e "${iface}, ${ip_private}, ${ip_public}"

    #### (2)
    # R1=$(cat /sys/class/net/${iface}/statistics/rx_bytes)
    # T1=$(cat /sys/class/net/${iface}/statistics/tx_bytes)
    # sleep 1
    # R2=$(cat /sys/class/net/${iface}/statistics/rx_bytes)
    # T2=$(cat /sys/class/net/${iface}/statistics/tx_bytes)
    # TBPS=$(expr $T2 - $T1)
    # RBPS=$(expr $R2 - $R1)
    # TKBPS=$(expr $TBPS / 1024)
    # RKBPS=$(expr $RBPS / 1024)

    # echo -e "${iface} tx ${TKBPS} kB/s rx ${RKBPS} kB/s, ${ip_private}, ${ip_public}"
}

#### time
function __time()
{
    date="$(date +"%a %Y-%m-%d %H:%M:%S")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo -e "${date}"
    [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${ORANGE}${date}${NORMAL}"
}

#### spaces
function __spaces()
{
    space=""
    for (( i=1; i<=${DEFAULT_SPACES}; i++))
    do
        space="${space} "
    done
    echo "${space}"
}

#### MAIN
while true;
do
    #### xsetroot
    if [[ ${STATUSCOLOR} -eq 0 ]]
    then
        # xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__weather) | $(__network) | $(__time) ]$(__spaces)"
        # xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__time) ]"
        xsetroot -name "[ $(__load) | $(__memory) | $(__battery) | $(__time) ]"
    elif [[ ${STATUSCOLOR} -eq 1 ]]
    then
        # xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__weather) | $(__network) | $(__time) ] $(__spaces)"
        xsetroot -name "${NORMAL}[ $(__load) | $(__memory) | $(__battery) | $(__time) ]${NORMAL}"
    fi
    sleep 1
done

#### END
