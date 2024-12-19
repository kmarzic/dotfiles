#!/bin/bash - 
#===============================================================================
#
#          FILE: rsyncscully2usb.sh
#
#         USAGE: ./rsyncscully2usb.sh
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: Ericsson Nikola Tesla d.d.
#       CREATED: 2015-07-18 08:32:35
#      REVISION: ---
#===============================================================================

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

echo "rsyncscully2usb"

## ----------------------------------------------------------------------------
## Function
## ----------------------------------------------------------------------------

function __rsync()
{
    destination=${1}

    echo ""
    echo "# rsync -rltDvu --progress --delete ${destination} ."
    rsync -rltDvu --progress --delete ${destination} .
}

function __parameters()
{
    DEV="${1}"
    FULL="${2}"

    if [ -z ${DEV} ]
    then
        echo "DEV not defined!"
        exit 1;
    fi

    if [ -z ${2} ]
    then
        FULL=""
    else
        FULL="${2}"
    fi

    target="kmarzic@kantica"

    echo "DEV='${DEV}'"
    echo "FULL='${FULL}'"
}


## ----------------------------------------------------------------------------
## MAIN
## ----------------------------------------------------------------------------

echo "rsyncscully2usb"
echo "usage:"
echo   "${0} device full"
echo "example:"
echo   "${0} /media/usb.ntfs1"
echo   "${0} /media/usb.ntfs1 full"

__parameters "${1}" "${2}"

cd ${DEV}
__rsync ${target}:/data/arduino .
__rsync ${target}:/data/android .
__rsync ${target}:/data/backup .
__rsync ${target}:/data/boardgames .
__rsync ${target}:/data/books .
__rsync ${target}:/data/books.big .
__rsync ${target}:/data/comics .
__rsync ${target}:/data/comics.big .
__rsync ${target}:/data/docs .
####
[[ ! -z ${FULL} ]] && __rsync ${target}:/data/games .
####
__rsync ${target}:/data/languages .
__rsync ${target}:/data/media .
__rsync ${target}:/data/music .
__rsync ${target}:/data/photo .
__rsync ${target}:/data/photo.private .
__rsync ${target}:/data/photo.private.backup .
####
[[ ! -z ${FULL} ]] && __rsync ${target}:/data/podcasts .
####
__rsync ${target}:/data/projects .

exit 0

## end
