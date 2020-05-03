#!/bin/bash
export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

function __dropbox()
{
    dropbox_detect=$(ps -ef | grep -v "grep" | grep "dropbox" | wc -l)

    if [[ ${dropbox_detect} -eq 0 ]]
    then
        echo "# dropbox start &"
        [[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
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
        [[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &
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
    echo "    --transparent true --alpha 255 --tint 0x859900 --expand true --heighttype pixel --height 16 --monitor 1 --padding 1 &"
    [[ -e /usr/bin/trayer ]] && trayer --edge bottom --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true \
        --transparent true --alpha 255 --tint 0x859900 --expand true --heighttype pixel --height 16 --monitor 1 --padding 1 &
}

function __dunst()
{
    pkill dunst
    dunst -config ~/.config/dunst/dunstrc &
}

#### dwm status
dwm.status.sh &

#### trayer
# trayer.sh &
#### trayer
# __trayer
__dropbox
__nm_applet
__dunst

#### xrandr
screen_toggle.sh -x
screen_toggle.sh -s ansi

#### dwm
while true;
do
    #### Log stderror to a file
    dwm 2> ~/.dwm.log
    #### No error logging
    # dwm >/dev/null 2>&1
done
#### END
