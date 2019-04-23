#!/bin/bash

export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:/opt/ghc/bin"
export PATH="$PATH:$HOME/.cabal/bin"
PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

## Remove xmonad files
cd ~/.xmonad && rm -f xmonad.errors xmonad.hi xmonad.o xmonad-x86_64-linux xmonad.state xmonad

## (1) - do not use sandbox
# ghc --make xmonad.hs
# xmonad --recompile

## (2) - use sandbox
sandbox_dir=$(dirname ~/data/cabal/xmonad/cabal.sandbox.config)
cabal --require-sandbox --sandbox-config-file=${sandbox_dir}/cabal.sandbox.config v1-exec -- ghc --make xmonad.hs

## END
