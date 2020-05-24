#!/bin/bash
export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

DEFAULT_SPACES=7
RED='\033[31m'
GREEN='\033[32m'
YELLOW='\033[33m'
BOLD='\033[1m'
NORMAL='\033[m'
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"

#### load
function __load()
{
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    echo "CPU: ${load}%"
}

#### temp
function __temp()
{
    # temp="$(sensors | grep "CPU:" | awk {' print $2 '})"
    temp="$(acpi -t | awk '{ print $4 " °C" }')"
    echo "Temp: ${temp}"
}

#### memory
function __memory()
{
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_percent=$(echo "scale=2; ${mem_free} / ${mem_total} * 100" | bc)
    echo "MEM: ${mem_percent%.*}%"
}

#### battery
function __battery()
{
    if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
    then
        battery="D $(acpi --battery | cut -d, -f2 | sed -e "s/ //g")"
    elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
    then
        battery="C $(acpi --battery | cut -d, -f2 | sed -e "s/ //g")"
    else
        battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g")"
    fi
    echo "BAT: ${battery}"
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
            echo -e "$(cat ${WTTR_FILE})"
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

#### time
function __time()
{
    date="$(date +"%a %Y-%m-%d %H:%M:%S")"
    echo "${date}"
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
    # xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__weather) | $(__time) ]$(__spaces)"
    xsetroot -name "[ $(__load) | $(__temp) | $(__memory) | $(__battery) | $(__time) ]"
    sleep 1
done

#### END
