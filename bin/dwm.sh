#!/usr/bin/env bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### xrandr
[[ -e ${HOME}/bin/screen.toggle.sh ]] && screen.toggle.sh -x

#### themes
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s ansi
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-atelier-lakeside-light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-google-light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-gruvbox-dark-soft
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s doom-one
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s dracula
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s everforest
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s gruvbox
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s gruvbox.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s monokai
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s nord
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s papercolor.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.white
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s srcery

#### dwm status - sync
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t ansi &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t base16-atelier-lakeside-light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t base16-google-light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t base16-gruvbox-dark-soft &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t doom-one &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t dracula &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t everforest &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t gruvbox.dark &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t gruvbox.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t monokai &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t nord &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t papercolor.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t selenized.dark &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t selenized.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t selenized.white &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t solarized.dark &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t solarized.light &
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s -t srcery &

#### dwm status - async
[[ -e ${HOME}/bin/dwmblocks ]] && dwmblocks &

#### dwm battery
[[ -e ${HOME}/bin/dwm.battery.sh ]] && dwm.battery.sh &

#### trayer
# [[ -e ${HOME}/bin/trayer.sh ]] && ${HOME}/bin/trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &
[[ -e /bin/globalprotect ]] && /bin/globalprotect launch-ui &

#### dwm
while true;
do
    #### Log stderror to a file
    ${HOME}/bin/dwm 2> ~/.dwm.log

    #### No error logging
    # ${HOME}/bin/dwm >/dev/null 2>&1
done

#### END
