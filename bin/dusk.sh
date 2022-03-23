#!/usr/bin/env bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### dusk status
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s ansi &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s base16-atelier-lakeside-light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s base16-google-light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s base16-gruvbox-dark-soft &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s doom-one &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s dracula &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s everforest &
[[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s gruvbox &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s gruvbox.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s monokai &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s nord &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s papercolor.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s solarized.dark &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s solarized.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s srcery &

#### dusk battery
[[ -e ${HOME}/bin/dusk.battery.sh ]] && dusk.battery.sh &

#### trayer
# [[ -e ${HOME}/bin/trayer.sh ]] && ${HOME}/bin/trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &
[[ -e /bin/globalprotect ]] && /bin/globalprotect launch-ui &

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
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.dark
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s solarized.light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s srcery

#### dusk
duskc run_command xrdb

while true;
do
    #### Log stderror to a file
    ${HOME}/bin/dusk 2> ~/.dusk.log

    #### No error logging
    # ${HOME}/bin/dusk >/dev/null 2>&1
done

#### END
