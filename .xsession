#!/bin/bash
# echo "G$(date) .xsesssion" >> /var/tmp/x

#### ENV
export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
export PATH="$PATH:/opt/ghc/bin"
# export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/fvwm3/bin"
export PATH="$PATH:/opt/fluxbox/bin"
export PATH="$PATH:/opt/icewm/bin"
export PATH="$PATH:/opt/lua/bin:/opt/luarocks/bin"
export PATH="$PATH:/opt/awesome/bin"
export PATH="$PATH:/opt/rofi/bin"
export PATH="$PATH:/opt/stumpwm/bin"
export PATH="$PATH:$HOME/bin"
# PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

export LUA_DIR=/opt/lua
export LUA_INCLUDE_DIR=$LUA_DIR/include
eval $(luarocks path)

export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"

export TERMINAL=urxvt
# export DBUS_SYSTEM_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket
# export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket

#### xprofile
# [ -f ~/.xprofile ] && . ~/.xprofile

#### Clean .xsesssion-errors
# echo "" > ~/.xsession-errors

#### awesome
# exec awesome || exec urxvt

#### bspwm
# exec bspwm || exec urxvt

#### dwm
# exec dwm.sh || exec urxvt

#### fluxbox
# exec fluxbox || exec urxvt

#### frankenwm
# exec frankenwm || exec urxvt

#### fvwm3
# exec fvwm3 || exec urxvt

## herbstluftwm
exec herbstluftwm || exec urxvt
# exec herbstluftwm.sh || exec urxvt

#### i3
# exec i3 || exec xterm

#### icewm
# exec icewm-session || exec xterm

#### sctortwm or scrotwm or spectrwm
# exec spectrwm || exec urxvt

#### stumpwm
# exec stumpwm || exec urxvt

#### xmonad
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.ansi.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.dark.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.light.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.ansi.hs xmobar.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.dark.hs xmobar.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.light.hs xmobar.hs && cd -
## (1)
# cd ~/data/cabal/xmonad
# cabal v1-exec -- xmonad --recompile
# cd ~/
## (2)
# xmonad --recompile
## (3) stack
# exec xmonad || exec urxvt

#### END
