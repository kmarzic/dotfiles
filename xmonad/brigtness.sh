#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

set -o nounset
set -e
set -u
set -o pipefail
IFS=$'\n\t'

DEFAULT_STEP=10

function __brightness()
{
    arg=${1}
    # echo "arg='${arg}'"

    if [[ -e /usr/bin/xbacklight ]]
    then
        [[ "${arg}" == "inc" ]] && /usr/bin/xbacklight -inc ${DEFAULT_STEP}
        [[ "${arg}" == "dec" ]] && /usr/bin/xbacklight -dec ${DEFAULT_STEP}
    fi

    if [[ -e /usr/bin/light ]]
    then
        [[ "${arg}" == "inc" ]] && /usr/bin/light -A ${DEFAULT_STEP}
        [[ "${arg}" == "dec" ]] && /usr/bin/light -U ${DEFAULT_STEP}
    fi
}

case "${1-}" in
    inc)
        [[ ! -z ${2-} ]] && DEFAULT_STEP=${2-}
        __brightness "inc"
        ;;
    dec)
        [[ ! -z ${2-} ]] && DEFAULT_STEP=${2-}
        __brightness "dec"
        ;;
    *)
       echo "Usage: $0 {inc|dec}"
       exit 1
       ;;
esac

exit 0

## END
