#!/bin/bash

export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

function __scratchpad_urxvt()
{
    xdotool search --onlyvisible --classname scratchpad windowunmap \
      || xdotool search --classname scratchpad windowmap \
      || urxvt -title "scratchpad" -name "scratchpad" -geometry 140x40+100-100 -fn "xft:monospace:pixelsize=14:antialias=true:style=bold"
}

function __scratchpad_st()
{
    xdotool search --onlyvisible --classname scratchpad windowunmap \
      || xdotool search --classname scratchpad windowmap \
      || st -T "scratchpad" -t "scratchpad" -n "scratchpad" -f "DejaVu Sans Mono:pixelsize=14:style=regular"
}

function __scratchpad_alacritty()
{
    xdotool search --onlyvisible --classname scratchpad windowunmap \
      || xdotool search --classname scratchpad windowmap \
      || alacritty -t "scratchpad" --class "scratchpad"
}

function __scratchpad_4()
{
    windowname="scratchpad"
    desk=$(wmctrl -d | grep '*' | cut -d ' ' -f '1')
    scratch_desk=$(wmctrl -l | grep " ${windowname}$" | cut -d ' ' -f '3')

    # xprop -name "$windowname"
    xprop -id $(wmctrl -l | grep " ${windowname}$" | awk '{ print $1 }')

    if [ ${?} -eq 0 ];
    then
        if [ ${desk} -eq ${scratch_desk} ];
        then
            wmctrl -r ${windowname} -t ${desk}
            wmctrl -r ${windowname} -b toggle,hidden
        else
            wmctrl -R ${windowname}
            wmctrl -r ${windowname} -b remove,hidden
        fi
    else
        urxvt -title "scratchpad" -name "scratchpad" -geometry 120x40+100-100 -fn "xft:Monospace:pixelsize=14:antialias=true:style=bold"
        # st -T ${windowname} -t ${windowname} -n ${windowname}
    fi
}

#### MAIN
# __scratchpad_urxvt
__scratchpad_st
# __scratchpad_alacritty
# __scratchpad_4

#### END
