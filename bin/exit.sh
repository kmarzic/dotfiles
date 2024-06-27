#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:${HOME}/bin

## ----------------------------------------------------------------------------
## Function
## ----------------------------------------------------------------------------

function __lock()
{
    #### i3lock
    # i3lock -i /opt/elx/share/wallpapers/elx_wallpaper_blue.png -d 5 -b
    # i3lock -i /opt/elx/share/wallpapers/elx_wallpaper_blue.png -n -p default
    #### black
    # i3lock -c 000000
    #### dark green
    # i3lock -c 073642
    #### gray
    # i3lock -c 3f3f3f
    #### blue
    # i3lock -c 005088

    #### i3lock bluring
    if [[ "$(hostname)" == "entd0001" ]] || [["$(hostname)" == "entl0002" ]] || [[ "$(hostname)" == "elx-5cg4126xwd" ]]
    then
        rm -f /var/tmp/screen_locked.png /var/tmp/screen_locked2.png
        scrot /var/tmp/screen_locked.png
        convert /var/tmp/screen_locked.png -scale 10% -scale 1000% /var/tmp/screen_locked2.png
        i3lock -i /var/tmp/screen_locked2.png
    fi

    #### slock bluring
    if [[ "$(hostname)" == "dle6440" ]] || [[ "$(hostname)" == "dle5570" ]] | [[ "$(hostname)" == "hp8560w" ]]
    then
        ~/bin/slock
    fi

    #### turn off monitor
    sleep 10 && xset dpms force off
}

function __monitor_off()
{
    xset dpms force off
}

function __logout()
{
    ## xmonad
    [[ $(ps -ef | grep "xmonad-x86_64-linux" | grep -v "grep" | wc -l) -gt 0 ]] && pkill xmonad

    ## awesome
    [[ $(ps -ef | grep "awesome" | grep -v "grep" | wc -l) -gt 0 ]] && pkill awesome

    ## dwm
    [[ $(ps -ef | grep "dwm.status.sh" | grep "bash" | grep -v "grep" | wc -l) -gt 0 ]] && kill $(ps -ef | grep "dwm.status.sh" | grep "bash" | grep -v "grep" | awk '{ print $2 }')
    [[ $(ps -ef | grep "dwm.sh" | grep "bash" | grep -v "grep" | wc -l) -gt 0 ]] && kill $(ps -ef | grep "dwm.sh" | grep "bash" | grep -v "grep" | awk '{ print $2 }')

    ## dusk
    [[ $(ps -ef | grep "dusk.status.sh" | grep "bash" | grep -v "grep" | wc -l) -gt 0 ]] && kill $(ps -ef | grep "dusk.status.sh" | grep "bash" | grep -v "grep" | awk '{ print $2 }')
    [[ $(ps -ef | grep "dusk.sh" | grep "bash" | grep -v "grep" | wc -l) -gt 0 ]] && kill $(ps -ef | grep "dusk.sh" | grep "bash" | grep -v "grep" | awk '{ print $2 }')

    ## i3
    [[ $(which i3-msg | wc -l) -ne 0 ]] && i3-msg exit

    ## herbstluftwm
    [[ $(ps -ef | grep "herbstluftwm.sh" | grep -v "grep" | wc -l) -ne 0 ]] && pkill herbsluftwm
    [[ $(ps -ef | grep "herbstluftwm" | grep -v "grep" | wc -l) -ne 0 ]] && herbstclient quit

    ## bspwm
    [[ $(ps -ef | grep "panel" | grep -v "grep" | wc -l) -ne 0 ]] && pkill -x panel; pkill -x panel
    [[ $(which bspc | wc -l) -ne 0 ]] && bspc quit
    [[ $(which bspc | wc -l) -ne 0 ]] && bspc quit 1
}

function __suspend()
{
    ## (1)
    # __lock && dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 "org.freedesktop.login1.Manager.Suspend" boolean:true
    ## (2)
    __lock && sudo systemctl suspend
}

function __hibernate()
{
    ## (1)
    # __lock && dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 "org.freedesktop.login1.Manager.Hibernate" boolean:true
    ## (2)
    __lock && sudo systemctl hybrid-sleep
}

function __reboot()
{
    ## (1)
    # dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 "org.freedesktop.login1.Manager.Reboot" boolean:true
    ## (2)
    sudo systemctl reboot
}

function __shutdown()
{
    ## (1)
    # dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 "org.freedesktop.login1.Manager.PowerOff" boolean:true
    ## (2)
    sudo systemctl poweroff
}

function __message()
{
    title="Exit"
    timeout="10"

    lock=",Lock:4"
    monitoroff=",Monitor Off:5"
    logout=",Logout:6"
    suspend=",Suspend:7"
    hibernate=",Hibernate:8"
    reboot=",Reboot:9"
    shutdown=",Shutdown:10"

    if [ -x /usr/bin/gxmessage ];
    then
        xmessage=/usr/bin/gxmessage
        cancel=",GTK_STOCK_CANCEL:11"
    else
        xmessage=/usr/bin/xmessage
        cancel=",Cancel:11"
    fi

    ${xmessage} -font fixed -buttons "${title}:3${lock}${monitoroff}${logout}${suspend}${hibernate}${reboot}${shutdown}${cancel}" \
        -default "${lock}" -timeout ${timeout} -center -name "Exit" \
        "Will cancel after ${timeout} seconds!"
    code=$?

    case "${code}" in
        0 | 3 | 11)
            ## Timeout
            ## Exit
            ## Cancel
            ;;
        4)
            ## Lock
            __lock
            ;;
        5)
            ## Monitor Off
            __monitor_off
            ;;
        6)
            ## Logout
            __logout
            ;;
        7)
            ## Suspend
            __suspend
            ;;
        8)
            ## Hibertnate
            __hibernate
            ;;
        9)
            ## Reboot
            __reboot
            ;;
        10)
            ## Shutdown
            __shutdown
            ;;
    esac
}

## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------

case "${1}" in
    lock)
        __lock
        ;;
    monitor_off)
        __monitor_off
        ;;
    logout)
        __logout
        ;;
    suspend)
        __suspend
        ;;
    hibernate)
        __hibernate
        ;;
    reboot)
        __reboot
        ;;
    shutdown)
        __shutdown
        ;;
    message)
        __message
        ;;
    *)
        echo "Usage: $0 { lock | monitor_off | logout | suspend | hibernate | reboot | shutdown | message }"
        exit 2
esac

exit 0

## ----------------------------------------------------------------------------
## END
## ----------------------------------------------------------------------------
