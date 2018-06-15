#!/bin/bash

# export __PATH_ORIG="$PATH"
# export PATH="$HOME/bin"
# export PATH="$PATH:/opt/ghc/bin:$HOME/.cabal/bin"
# export PATH="$PATH:$__PATH_ORIG"

## (1) - do not use sandbox
# xmonad --recompile
# xmonad --restart

## (2) - use sandbox
# cd ~/data/cabal/xmonad
# cd ~/data/cabal/xmonad.dev.20180223
# cd ~/data/cabal/xmonad.dev.20180320
# cd ~/data/cabal/xmonad.dev.20180410
# cd ~/data/cabal/xmonad.dev.20180420
# cd ~/data/cabal/xmonad.dev.20180514
# cd ~/data/cabal/xmonad.dev.20180530
# cd ~/data/cabal/xmonad.dev.20180609
cd ~/data/cabal/xmonad.dev.20180610
cabal exec -- xmonad --recompile
cabal exec -- xmonad --restart

## END
