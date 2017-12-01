#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

function __detect_wireless_1()
{
    essid=`nmcli -t -f active,ssid dev wifi | egrep '^yes' | cut -d\' -f2`
    stngth=`nmcli -t -f active,ssid,signal dev wifi|grep yes|cut -d':' -f3`
    bars=`expr $stngth / 10`
}

function __detect_wireless_2()
{
    IFS=$'\n'
    DEVICE=( $(nmcli connection show --active | grep "wireless" | awk '{ print $1 }') )
    IFS="${oldifs}"

    DEVICE_length=${#DEVICE[@]}

    if [[ ${DEVICE_length} -eq 0 ]]
    then
        echo "wired"
        exit 0
    fi

    for (( i=0; i<${#DEVICE[@]}; i++ ));
    do
        # echo "device[${i}]='${DEVICE[i]}'";

        essid=$(nmcli -t -f active,ssid dev wifi | egrep "^yes" | awk -F "yes:" '{ print $2 }')
        # echo "essid=${essid}"

        quality=$(nmcli -t -f active,ssid,signal dev wifi | grep "yes" | awk -F ":" '{ print $3 }')
        # echo "quality=${quality}"

        bars=$(printf %.$2f $(echo "scale=2; ${quality}/10" | bc))
        # echo "bars=${bars}"
    done
}

function __print_bar()
{
    case ${bars} in
        0)  bar='[----------]' ;;
        1)  bar='[/---------]' ;;
        2)  bar='[//--------]' ;;
        3)  bar='[///-------]' ;;
        4)  bar='[////------]' ;;
        5)  bar='[/////-----]' ;;
        6)  bar='[//////----]' ;;
        7)  bar='[///////---]' ;;
        8)  bar='[////////--]' ;;
        9)  bar='[/////////-]' ;;
        10) bar='[//////////]' ;;
        *)  bar='[----!!----]' ;;
    esac

    echo ${essid} ${bar}
}

## Main
# __detect_wireless_1
__detect_wireless_2
__print_bar

exit 0

## END
