#!/usr/bin/env bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### dwm status
[[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh &

#### dwm battery
[[ -e ${HOME}/bin/dwm.battery.sh ]] && dwm.battery.sh &

#### trayer
# trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &

#### xrandr
[[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -x
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s ansi
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s dracula
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s monokai
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s nord
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s solarized.dark
# [[ -e ${HOME}/bin/screen_toggle.sh ]] && screen_toggle.sh -s solarized.light

#### dwm
while true;
do
    #### Log stderror to a file
    ${HOME}/bin/dwm 2> ~/.dwm.log

    #### No error logging
    # ${HOME}/bin/dwm >/dev/null 2>&1
done

#### END
