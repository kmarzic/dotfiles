#!/bin/sh

#######################################################################
## Environment Variables
#######################################################################
export __PATH_ORIG="$PATH"
export PATH="$HOME/bin"
export PATH="$PATH:/opt/ghc/bin"
export PATH="$PATH:$HOME/.cabal/bin"
export PATH="$PATH:/opt/perl5/bin:/opt/perl5/cpan/bin"
export PATH="$PATH:/opt/python3/bin:/opt/python3.ext/bin"
export PATH="$PATH:$__PATH_ORIG"
PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"


#######################################################################
## .xprofile
#######################################################################

[ -f ~/.xprofile ] && . ~/.xprofile


#######################################################################
## Window Managers
#######################################################################

## Clean .xsesssion-errors
echo "" > ~/.xsession-errors

## 1. openbox
# exec openbox-session || exec xterm

## 2. fvwm
# exec fvwm2 || exec xterm
# exec /opt/fvwm26/bin/fvwm2 || exec xterm

## 3. icewm
# exec icewm-session || exec xterm
# exec /opt/icewm13/bin/icewm-session || exec xterm

## 4. fluxbox
# exec fluxbox || exec xterm

## 5. gnome
# exec gnome-session || exec xterm

## 6. xfce4
# exec xfce4-session || exec xterm

## 7. tinywm
# exec tinywm || exec xterm

## 8. pekwm
# exec pekwm || exec xterm

## 9. jwm
# exec jwm || exec xterm

## 10. i3
# exec i3 -d all || exec xterm
# exec i3 || exec xterm

## 11. sctortwm or scrotwm
# exec spectrwm || exec xterm

## 12. herbstluftwm
# exec herbstluftwm || exec xterm

## 13. awesome
# exec awesome || exec xterm
# exec awesome -c ~/.config/awesome/rc.lua || exec xterm

## 14. dwm
# exec dwm || exec xterm

## 15. wmii
# exec wmii || exec xterm

## 16. xmonad
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.dark.hs xmonad.hs && cd -
[[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.light.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.dark.hs xmobar.hs && cd -
[[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.light.hs xmobar.hs && cd -
## (1)
# cd ~/data/cabal/xmonad
# cd ~/data/cabal/xmonad.dev.20180610
# cd ~/data/cabal/xmonad.dev.20180620
cd ~/data/cabal/xmonad.dev.20180714
cabal exec -- xmonad --recompile
cd ~/
## (2)
# xmonad --recompile
##
exec xmonad || exec xterm

## 17. terminals
# exec xterm
# exec urxvt

#######################################################################
## END
#######################################################################
