#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

function __dropbox()
{
    dropbox_detect=$(ps -ef | grep -v "grep" | grep "dropbox" | wc -l)

    if [[ ${dropbox_detect} -eq 0 ]]
    then
        echo "# dropbox start &"
        dropbox start &
    else
        echo "dropbox is running"
    fi
}

function __nm_applet()
{
    nmapplet_detect=$(ps -ef | grep -v "grep" | grep "nm-applet" | wc -l)

    if [[ ${nmapplet_detect} -eq 0 ]]
    then
        echo "# nm-applet &"
        nm-applet &
    else
        echo "nm-applet is running"
    fi
}

function __trayer()
{
    IFS=$'\n'
    ps_trayer=( $(ps -ef | grep "trayer --edge" | grep -v "grep" | awk '{ print $2 }') )
    IFS="${oldifs}"

    for (( i=0; i<${#ps_trayer[@]}; i++ ));
    do
        echo "# kill ${ps_trayer[i]}"
        kill ${ps_trayer[i]}
    done

    sleep 1

    echo "# trayer --edge bottom --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true "
    echo "    --transparent true --alpha 0 --tint 0x1A1918 --expand true --heighttype pixel --height 16 --monitor 1 --padding 1 &"
    trayer --edge bottom --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true \
        --transparent true --alpha 0 --tint 0x1A1918 --expand true --heighttype pixel --height 16 --monitor 1 --padding 1 &
}

# __dropbox
# __nm_applet
__trayer

## END
