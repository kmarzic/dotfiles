#!/usr/bin/env bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### Variables
OS=$(uname -s)

#### Functions
function __dusk_battery_linux()
{
    while true;
    do
        #### capacity
        capacity="$(cat /sys/class/power_supply/BAT*/capacity | head -1)"
        [ -z "$capacity" ] && break

        #### status
        batstatus=$(cat /sys/class/power_supply/BAT*/status | head -1)

        #### notification
        if [ "${batstatus}" = "Discharging" ] && [ "${capacity}" -lt 25 ];
        then
            notify-send --urgency=CRITICAL "battery low" "${capacity}%" &
        fi

        #### sleep
        sleep 60
    done
}

function __dusk_battery_freebsd()
{
    while true;
    do
        #### capacity
        capacity="$(sysctl hw.acpi.battery | grep hw.acpi.battery.life | awk -F ' ' '{ print $2 }')"

        #### sleep
        sleep 60
    done
}

#### MAIN
IAM=(`pgrep -d " " -f ${0//*\//}`)
[ ${#IAM[@]} -gt 1 ] && { echo I AM running; exit 127; } || echo "I AM not running. Running now ;-p"

if [[ "${OS}" = "Linux" ]]
then
    if [[ ${dusk_battery_detect} -eq 2 ]]
    then
        __dusk_battery_linux
    else
        echo "dusk.battery.sh is running"
    fi
elif [[ "${OS}" = "FreeBSD" ]]
then
    if [[ ${dusk_battery_detect} -eq 0 ]]
    then
        __dusk_battery_freebsd
    else
        echo "dusk.battery.sh is running"
    fi
fi

#### END
