#!/bin/bash
export PATH=$HOME/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

DEFAULT_SPACES=7
RED='^c#ff0000^'
GREEN='^c#00ff00^'
YELLOW='^c#ffff00^'
CYAN='^c#00ffff^'
NORMAL='^c#bbbbbb^'
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"
STATUSCOLOR=1

#### load
function __load()
{
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_dec_temp="$(cat /proc/loadavg | awk {' print $1 '} | awk -F "." '{ print $2 }')"
    load_dec="$(echo ${load_dec_0} | bc)"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "CPU: ${load}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${load_dec} -lt 50 ]] && echo "CPU: ${GREEN}${load}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${load_dec} -gt 50 ]] && [[ ${load_dec} -lt 80 ]] && echo "CPU: ${YELLOW}${load}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${load_dec} -gt 80 ]] && echo "CPU: ${RED}${load}%${NORMAL}"
}

#### temp
function __temp()
{
    temp="$(acpi -t | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | sed -e "s/\..*//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "Temp: ${temp}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${temp_dec} -lt 50 ]] && echo "Temp: ${GREEN}${temp}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${temp_dec} -gt 40 ]] && [[ ${temp_dec} -lt 60 ]] && echo "Temp: ${YELLOW}${temp}${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${temp_dec} -gt 60 ]] && echo "Temp: ${RED}${temp}${NORMAL}"
}

#### memory
function __memory()
{
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_percent=$(echo "scale=2; ${mem_free} / ${mem_total} * 100" | bc | sed -e "s/\..*//g")

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "MEM: ${mem_percent}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${mem_percent} -lt 50 ]] && echo "MEM: ${GREEN}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${mem_percent} -gt 50 ]] && [[ ${mem_percent} -lt 90 ]] && echo "MEM: ${YELLOW}${mem_percent}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${mem_percent} -gt 90 ]] && echo "MEM: ${RED}${mem_percent}%${NORMAL}"
}

#### battery
function __battery()
{
    if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
    then
        [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="D"
        [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${CYAN}D${NORMAL}"
    elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
    then
        [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="C"
        [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${CYAN}C${NORMAL}"
    else
        [[ ${STATUSCOLOR} -eq 0 ]] && battery_status="F"
        [[ ${STATUSCOLOR} -eq 1 ]] && battery_status="${CYAN}F${NORMAL}"
    fi
    battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

    [[ ${STATUSCOLOR} -eq 0 ]] && echo "BAT: ${battery_status}${battery}%"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -lt 25 ]] && echo "BAT: ${battery_status} ${RED}${battery}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 25 ]] && [[ ${battery} -lt 80 ]] && echo "BAT: ${battery_status} ${YELLOW}${battery}%${NORMAL}"
    [[ ${STATUSCOLOR} -eq 1 ]] && [[ ${battery} -gt 80 ]] && echo "BAT: ${battery_status} ${GREEN}${battery}%${NORMAL}"
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
    [[ ${STATUSCOLOR} -eq 1 ]] && echo -e "${CYAN}${date}${NORMAL}"
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
    # xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__weather) | $(__network) | $(__time) ]$(__spaces)"
    xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__weather) | $(__time) ]"
    sleep 1
done

#### END
