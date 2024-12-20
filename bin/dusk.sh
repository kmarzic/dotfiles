#!/usr/bin/env bash

export PATH="${HOME}/bin:${HOME}/.local/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

#### xrandr
[[ -e ${HOME}/bin/screen.toggle.sh ]] && screen.toggle.sh -x

#### themes
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s ansi
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-atelier-lakeside-light
# [[ -e ${HOME}/bin/screen.togggle.sh ]] && screen.togggle.sh -s base16-atelier-savanna-light
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

#### dusk status - sync
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t ansi &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t base16-atelier-lakeside-light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t base16-atelier-savanna-light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t base16-google-light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t base16-gruvbox-dark-soft &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t doom-one &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t dracula &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t everforest &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t gruvbox.dark &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t gruvbox.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t monokai &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t nord.dark &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t nord.light&
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t papercolor.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t selenized.dark &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t selenized.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t selenized.white &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t solarized.dark &
[[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t solarized.light &
# [[ -e ${HOME}/bin/dusk.status.sh ]] && dusk.status.sh -s -t srcery &

#### dusk status - async

#### dusk battery
[[ -e ${HOME}/bin/dusk.battery.sh ]] && dusk.battery.sh &

#### trayer
# [[ -e ${HOME}/bin/trayer.sh ]] && ${HOME}/bin/trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &
[[ -e /bin/globalprotect ]] && /bin/globalprotect launch-ui &

#### dusk
while true;
do
    #### Log stderror to a file
    [[ -e ${HOME}/.local/bin/dusk ]] && ${HOME}/.local/bin/dusk 2> ~/.dusk.log
    # [[ -e ${HOME}/bin/dusk ]] && ${HOME}/bin/dusk 2> ~/.dusk.log

    #### No error logging
    # [[ -e ${HOME}/.local/bin/dusk ]] && ${HOME}/.local/bin/dusk >/dev/null 2>&1
    # [[ -e ${HOME}/bin/dusk ]] && ${HOME}/bin/dusk >/dev/null 2>&1
done

#### END
