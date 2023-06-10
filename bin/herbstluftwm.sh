#!/bin/bash
export PATH=$HOME/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

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

#### trayer
# [[ -e ${HOME}/bin/trayer.sh ]] && ${HOME}/bin/trayer.sh &
[[ -e /usr/bin/dropbox ]] && /usr/bin/dropbox start &
[[ -e /usr/bin/nm-applet ]] && /usr/bin/nm-applet &
[[ -e /bin/globalprotect ]] && /bin/globalprotect launch-ui &

#### herbsluftwm
while true;
do
    #### Log stderror to a file
    herbstluftwm 2> ~/.herbstluftwm.log

    #### No error logging
    # herbstluftwm >/dev/null 2>&1
done

#### END
