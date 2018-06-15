#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin
DEFAULT_SPACES=5

function __echo_spaces()
{
    for (( i=1; i<=${DEFAULT_SPACES}; i++))
    do
        echo -n " "
    done
}

case "${1}" in
    spaces)
        if [[ ! -z ${2-} ]]
        then
            DEFAULT_SPACES=${2-}
           __echo_spaces
        fi
        ;;
    *)
        __echo_spaces
esac
exit 0

## END
