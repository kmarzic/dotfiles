#!/usr/bin/env bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### dwm status
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s ansi &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s base16-atelier-lakeside-light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s base16-google-light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s base16-gruvbox-dark-soft &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s dracula &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s everforest &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s gruvbox &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s monokai &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s nord &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s papercolor.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s solarized.dark &
[[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s solarized.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s srcery &

#### dwm battery
[[ -e ${HOME}/bin/dwm.battery.sh ]] && dwm.battery.sh &

#### trayer
# trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &

#### xrandr
[[ -e ${HOME}/bin/screen.toggle.sh ]] && screen.toggle.sh -x

#### themes
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s ansi
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-atelier-lakeside-light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-google-light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-gruvbox-dark-soft
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s dracula
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s everforest
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s gruvbox
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s monokai
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s nord
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s papercolor.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s srcery

#### dwm
while true;
do
    #### Log stderror to a file
    ${HOME}/bin/dwm 2> ~/.dwm.log

    #### No error logging
    # ${HOME}/bin/dwm >/dev/null 2>&1
done

#### END
