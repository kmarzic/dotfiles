#!/bin/bash

export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:/opt/ghc/bin"
export PATH="$PATH:$HOME/.cabal/bin"
# export PATH="$PATH:$HOME/.local/bin"
PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

## Remove xmonad files
cd ~/.xmonad && rm -f xmonad.errors xmonad.hi xmonad.o xmonad-x86_64-linux xmonad.state xmonad

## (1) - do not use sandbox
# ghc --make xmonad.hs

## (2) - use sandbox
# sandbox_dir=$(dirname ~/data/cabal/xmonad/cabal.sandbox.config)
# cabal --require-sandbox --sandbox-config-file=${sandbox_dir}/cabal.sandbox.config v1-exec -- ghc --make xmonad.hs

## (3) - stack
# stack --stack-yaml stack.yaml ghc -- --make xmonad.hs -i -fforce-recomp -main-is main -v0

## END
