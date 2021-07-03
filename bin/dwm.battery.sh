#!/bin/bash

export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"

function __dwm_battery()
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

#### MAIN
dwm_battery_detect=$(ps -ef | grep -v "grep" | grep "dwm.battery.sh" | wc -l)
echo "dwm_battery_detect='${dwm_battery_detect}'"

if [[ ${dwm_battery_detect} -eq 2 ]]
then
    __dwm_battery
else
    echo "dwm.battery.sh is running"
fi

#### END
