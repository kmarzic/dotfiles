#!/bin/bash
# echo "G$(date) .xsesssion" >> /var/tmp/x

#### ENV
export PATH="${HOME}/bin:${HOME}/.local/bin"
export PATH="${PATH}:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
# export PATH="${PATH}:/opt/ghc/bin"
# export PATH="${PATH}:${HOME}/.cabal/bin"
# export PATH="${PATH}:/opt/fvwm3/bin"
# export PATH="${PATH}:/opt/fluxbox/bin"
export PATH="${PATH}:/opt/icewm/bin"
# export PATH="${PATH}:/opt/lua/bin:/opt/luarocks/bin"
# export PATH="${PATH}:/opt/awesome/bin"
# export PATH="${PATH}:/opt/rofi/bin"
# export PATH="${PATH}:/opt/stumpwm/bin"
# PATH=$(echo "${PATH}" | awk -v RS=':' -v ORS=":" '!a[$1]++')

# export LUA_DIR=/opt/lua
# export LUA_INCLUDE_DIR=${LUA_DIR}/include
# eval $(luarocks path)

export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"

[[ -e ${HOME}/.local/bin/st ]] && export TERMINAL=st
[[ ! -e $HOME/.local/bin/st ]] && export TERMINAL=urxvt

# [[ -e /etc/arch-release ]] && export DBUS_SYSTEM_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket
# [[ -e /etc/arch-release ]] && export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket
[[ -e /etc/arch-release ]] && export XDG_RUNTIME_DIR=/run/user/${UID}/
[[ -e /etc/arch-release ]] && export $(dbus-launch)

#### xprofile
# [ -f ~/.xprofile ] && . ~/.xprofile

#### Clean .xsesssion-errors
# echo "" > ~/.xsession-errors

#### awesome
# exec awesome || exec urxvt

#### bspwm
# exec bspwm || exec urxvt

#### dusk
# exec dusk.sh || exec urxvt

#### dwm
exec dwm.sh || exec urxvt

#### fluxbox
# exec fluxbox || exec urxvt

#### frankenwm
# exec frankenwm || exec urxvt

#### fvwm3
# exec fvwm3 || exec urxvt

## herbstluftwm
# exec herbstluftwm.sh || exec urxvt

#### i3
# exec i3 || exec xterm

#### icewm
# exec icewm-session || exec xterm

#### leftwm
# exec leftwm || exec xterm

#### sctortwm or scrotwm or spectrwm
# exec spectrwm || exec urxvt

#### stumpwm
# exec stumpwm || exec urxvt

#### xmonad
## (1)
# cd ~/data/cabal/xmonad
# cabal v1-exec -- xmonad --recompile
# cd ~/
## (2)
# xmonad --recompile
## (3) stack
# exec xmonad || exec urxvt
# exec $HOME/.cache/xmonad/xmonad-x86_64-linux || exec urxvt

#### dwl (wayland)
# exec dwl || exec alacritty

#### sway (wayland)
# exec sway || exec alacritty

#### END
