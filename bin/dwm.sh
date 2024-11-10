#!/usr/bin/env bash

export PATH="${HOME}/bin:${HOME}/.local/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

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
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s nord.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s nord.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s papercolor.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s selenized.white
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s srcery

#### dwm status - sync
# [[ -e ${HOME}/bin/dwm.status.sh ]] && dwm.status.sh -s &

#### dwm status - async
[[ -e ${HOME}/.local/bin/dwmblocks ]] && dwmblocks &

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
    [[ -e ${HOME}/.local/bin/dwm ]] && ${HOME}/.local/bin/dwm 2> ~/.dwm.log
    # [[ -e ${HOME}/bin/dwm ]] && ${HOME}/bin/dwm 2> ~/.dwm.log

    #### No error logging
    # [[ -e ${HOME}/.local/bin/dwm ]] && ${HOME}/.local/bin/dwm >/dev/null 2>&1
    # [[ -e ${HOME}/bin/dwm ]] && ${HOME}/bin/dwm >/dev/null 2>&1
done

#### END
