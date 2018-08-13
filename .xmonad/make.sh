#!/bin/bash

# export __PATH_ORIG="$PATH"
# export PATH="$HOME/bin"
# export PATH="$PATH:/opt/ghc/bin:$HOME/.cabal/bin"
# export PATH="$PATH:$__PATH_ORIG"

## Remove xmonad files
cd ~/.xmonad
rm -f xmonad.errors xmonad.hi xmonad.o xmonad-x86_64-linux xmonad.state xmonad

## (1) - do not use sandbox
# ghc --make xmonad.hs
# xmonad --recompile

## (2) - use sandbox
# sandbox_dir=$(dirname ~/data/cabal/xmonad.dev.20180727/cabal.sandbox.config)
# sandbox_dir=$(dirname ~/data/cabal/xmonad.dev.20180803/cabal.sandbox.config)
# sandbox_dir=$(dirname ~/data/cabal/xmonad.dev.20180813/cabal.sandbox.config)
sandbox_dir=$(dirname ~/data/cabal/xmonad/cabal.sandbox.config)
cabal --require-sandbox --sandbox-config-file=${sandbox_dir}/cabal.sandbox.config exec -- ghc --make xmonad.hs

## END
