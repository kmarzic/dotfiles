#!/bin/bash -
#===============================================================================
#
#          FILE: rsyncfromip.sh
#
#         USAGE: ./rsyncfromip.sh <target>
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: Ericsson Nikola Tesla d.d.
#       CREATED: 2025-01-06 08:32:35
#      REVISION: ---
#===============================================================================

## ----------------------------------------------------------------------------
## Variables
## ----------------------------------------------------------------------------

#### PATH
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### Banner
BANNER="rsyncfromip"

#### Debug
DEBUG=0 # debug is off
# DEBUG=1 # debug is on

#### Exit
EXIT_OK=0
EXIT_ERROR=1

## ----------------------------------------------------------------------------
## Functions
## ----------------------------------------------------------------------------

function __printf()
{
    ## defining colors for outputs
    RED='\033[31m'
    GREEN='\033[32m'
    YELLOW='\033[33m'
    BOLD='\033[1m'
    NORMAL='\033[m'

    [ "${3-}" = "nolb" ] && ECHOSWITCH="-ne" || ECHOSWITCH="-e"

    if [[ ! -z ${2-} && ! -z ${1-} ]]
    then
        case ${2} in
            error)
                echo -e "${RED}${1}${NORMAL}" >&2
                ;;
            info)
                echo ${ECHOSWITCH} "${YELLOW}${1}${NORMAL}"
                ;;
            success)
                echo -e "${GREEN}${1}${NORMAL}"
                ;;
            header)
                echo -e "${BOLD}${1}${NORMAL}"
                ;;
            debug)
                [ ${DEBUG-} -eq 1 ] && echo -e "${1}"
                ;;
            log)
                if [ ${LOG_ENABLED-} ]
                then
                    if [ ! -d ${LOG_DIR} ]
                    then
                        mkdir ${LOG_DIR}
                    fi

                    echo -e "$(date +%Y%m%dT%H%M%S);${1}" >> ${LOG_FILE-}
                fi
                ;;
            *)
                echo -e "${1}"
                ;;
        esac
    else
        echo "${1}"
    fi
}

function __banner()
{
    __printf "${BANNER}" info
}

function __rsync()
{
    dir=${1}
    destination=${2}

    __printf ""
    __printf "# rsync -rltDvu --progress --delete ${dir} ${destination}" success
    rsync -rltDvu --progress --delete ${dir} ${destination}
}

function __help()
{
    __printf "usage:"
    __printf "  ${0} <target>"
    __printf "example:"
    __printf "  ${0} tkremar@entd0001"
    __printf "  ${0} tkremar@etk.ansible.entd0001"
    __printf "  ${0} tkremar@etk.ansible.pckrma"
    __printf "  ${0} tkremar@entl0002"
    __printf "  ${0} tkremar@etk.ansible.entl0002"
    __printf "  ${0} etkkrma@elx-5cg4126xwd"
    __printf "  ${0} kmarzic@scully"
}

## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------

__banner

target=${1}

if [ -z ${target} ]
then
    __help
    __printf "target not defined!" error
    exit ${EXIT_ERROR};
fi

case "${target}" in
    "tkremar@entd0001" | "tkremar@etk.ansible.entd0001" | "tkremar@etk.ansible.pckrma")
        __printf "rsyncfromip tkremar@entd0001" info

        cd ~/data
        __rsync ${target}:/home/tkremar/data/projects .
        __rsync ${target}:/home/tkremar/data/ssh .
        __rsync ${target}:/home/tkremar/data/certs .
        __rsync ${target}:/home/tkremar/data/vpn .
        __rsync ${target}:/home/tkremar/data/secrets .

        cd ~/
        __rsync ${target}:/home/tkremar/wallpapers .
        __rsync ${target}:/home/tkremar/bin .
        __rsync ${target}:/home/tkremar/Maildir .

        __printf "Done!" success
        ;;
    "tkremar@entl0002" | "tkremar@etk.ansible.entl0002")
        __printf "rsyncfromip tkremar@entd0002" info

        cd ~/data
        __rsync ${target}:/home/tkremar/data/projects .
        __rsync ${target}:/home/tkremar/data/ssh .
        __rsync ${target}:/home/tkremar/data/certs .
        __rsync ${target}:/home/tkremar/data/vpn .
        __rsync ${target}:/home/tkremar/data/secrets .

        cd ~/
        __rsync ${target}:/home/tkremar/wallpapers .
        __rsync ${target}:/home/tkremar/bin .
        __rsync ${target}:/home/tkremar/Maildir .

        __printf "Done!" success
        ;;
    "etkkrma@elx-5cg4126xwd")
        __printf "rsyncfromip etkkrma@elx-5cg4126xwd" info

        cd ~/data
        __rsync ${target}:/home/etkkrma/data/projects .
        __rsync ${target}:/home/etkkrma/data/ssh .
        __rsync ${target}:/home/etkkrma/data/certs .
        __rsync ${target}:/home/etkkrma/data/vpn .
        __rsync ${target}:/home/etkkrma/data/secrets .

        cd ~/
        __rsync ${target}:/home/etkkrma/wallpapers .
        __rsync ${target}:/home/etkkrma/bin .
        __rsync ${target}:/home/etkkrma/Maildir .

        __printf "Done!" success
        ;;
    "kmarzic@scully")
        __printf "rsyncfromip kmarzic@scully" info

        cd ~/data
        __rsync ${target}:/data/projects .

        cd ~/
        __rsync ${target}:/data/photo/wallpapers .

        __printf "Done!" success
        ;;
    *)
        __help
        ;;
esac

exit ${EXIT_OK}

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
