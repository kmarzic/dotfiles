#!/bin/bash

# export __PATH_ORIG="$PATH"
# export PATH="$HOME/bin"
# export PATH="$PATH:/opt/ghc/bin:$HOME/.cabal/bin"
# export PATH="$PATH:$__PATH_ORIG"

## (1) - do not use sandbox
xmonad --recompile
xmonad --restart

## (2) - use sandbox
# cd ~/data/cabal/xmonad
# cd ~/data/cabal/xmonad.dev.20180610
# cd ~/data/cabal/xmonad.dev.20180620
# cd ~/data/cabal/xmonad.dev.20180714
# Cabal exec -- xmonad --recompile
# Cabal exec -- xmonad --restart

## END
