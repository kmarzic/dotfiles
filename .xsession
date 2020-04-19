#!/bin/bash
# echo "G$(date) .xsesssion" >> /var/tmp/x

#### ENV
export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:/opt/ghc/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:/opt/fvwm3/bin"
export PATH="$PATH:/opt/awesome/usr/local/bin"
export PATH="$PATH:/opt/rofi/bin"
# PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"

#### xprofile
# [ -f ~/.xprofile ] && . ~/.xprofile

#### Clean .xsesssion-errors
echo "" > ~/.xsession-errors

#### i3
# exec i3 || exec xterm

#### xmonad
[[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.ansi.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.dark.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.light.hs xmonad.hs && cd -
[[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.ansi.hs xmobar.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.dark.hs xmobar.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.light.hs xmobar.hs && cd -
## (1)
# cd ~/data/cabal/xmonad
# cabal v1-exec -- xmonad --recompile
# cd ~/
## (2)
# xmonad --recompile
##
# exec xmonad || exec urxvt

#### sctortwm or scrotwm or spectrwm
# exec spectrwm || exec urxvt

#### awesome
exec awesome || exec urxvt

#### bspwm
# exec bspwm || exec urxvt

#### dwm
# exec dwm.sh || exec urxvt

#### dwm
# exec fvwm3 || exec urxvt

#### END
