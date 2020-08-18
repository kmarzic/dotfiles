#!/bin/bash
export PATH=$HOME/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### trayer
# trayer.sh &
# /usr/bin/dropbox start &
# /usr/bin/nm-applet &

#### xrandr
screen_toggle.sh -x
screen_toggle.sh -s ansi

#### herbsluftwm
while true;
do
    #### Log stderror to a file
    herbstluftwm 2> ~/.herbstluftwm.log

    #### No error logging
    # herbstluftwm >/dev/null 2>&1
done

#### END
