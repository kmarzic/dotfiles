#### xsession

#### Environment Variables
export __PATH_ORIG="$PATH"
export PATH="$HOME/bin"
# export PATH="$PATH:/opt/ghc/bin:$HOME/.cabal/bin"
export PATH="$PATH:/opt/perl5/bin:/opt/perl5/cpan/bin"
export PATH="$PATH:/opt/python3/bin:/opt/python3.ext/bin"
export PATH="$PATH:$__PATH_ORIG"

export LANG="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_TIME="en_DK.UTF-8"

#### .xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

#### i3wm
# exec i3wm || exec xterm

#### xmonad
# [[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.dark.hs xmonad.hs && cd -
[[ ! -e ~/.xmonad/xmonad.hs ]] && cd ~/.xmonad && ln -s xmonad.solarized.light.hs xmonad.hs && cd -
# [[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.dark.hs xmobar.hs && cd -
[[ ! -e ~/.xmonad/xmobar.hs ]] && cd ~/.xmonad && ln -s xmobar.solarized.light.hs xmobar.hs && cd -
## (1)
# cd ~/data/cabal/xmonad
# cd ~/data/cabal/xmonad.dev.20180223
# cd ~/data/cabal/xmonad.dev.20180320
# cd ~/data/cabal/xmonad.dev.20180410
# cd ~/data/cabal/xmonad.dev.20180420
# cabal exec -- xmonad --recompile
# cd ~/
## (2)
xmonad --recompile
##
exec xmonad || exec xterm

#### bspwm
# ~/bin/sxhkd &
# exec ~/bin/bspwm || exec xterm

#### END
