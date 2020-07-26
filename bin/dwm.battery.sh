#!/bin/bash

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
####  END
