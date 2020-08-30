#!/bin/bash
export PATH=$HOME/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### dwm status
dwm.status.sh &

#### dwm battery
dwm.battery.sh &

#### trayer
trayer.sh &
# /usr/bin/dropbox start &
# /usr/bin/nm-applet &

#### xrandr
screen_toggle.sh -x
screen_toggle.sh -s ansi
# screen_toggle.sh -s monokai
# screen_toggle.sh -s nord
# screen_toggle.sh -s solarized.dark
# screen_toggle.sh -s solarized.light

#### dwm
while true;
do
    #### Log stderror to a file
    dwm 2> ~/.dwm.log

    #### No error logging
    # dwm >/dev/null 2>&1
done

#### END
