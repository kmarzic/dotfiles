#!/bin/bash

# export __PATH_ORIG="$PATH"
# export PATH="$HOME/bin"
# export PATH="$PATH:/opt/ghc/bin:$HOME/.cabal/bin"
# export PATH="$PATH:$__PATH_ORIG"

## (1) - do not use sandbox
# xmonad --recompile
# xmonad --restart

## (2) - use sandbox
cd ~/data/cabal/xmonad
cabal exec -- xmonad --recompile
cabal exec -- xmonad --restart

## END
