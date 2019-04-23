#!/bin/bash

export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:/opt/ghc/bin"
export PATH="$PATH:$HOME/.cabal/bin"
PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++')

## (1) - do not use sandbox
# xmonad --recompile
# xmonad --restart

## (2) - use sandbox
# cd ~/data/cabal/xmonad
# cabal v1-exec -- xmonad --recompile
# cabal v1-exec -- xmonad --restart

## (3) - use sandbox
sandbox_dir=$(dirname ~/data/cabal/xmonad/cabal.sandbox.config)
cabal --require-sandbox --sandbox-config-file=${sandbox_dir}/cabal.sandbox.config v1-exec -- xmonad --recompile
cabal --require-sandbox --sandbox-config-file=${sandbox_dir}/cabal.sandbox.config v1-exec -- xmonad --restart

## END
