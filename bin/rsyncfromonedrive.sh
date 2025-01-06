#!/bin/bash -
#===============================================================================
#
#          FILE: rsyncfromonedrive.sh
#
#         USAGE: ./rsyncfromonedrive.sh <target>
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
BANNER="rsyncfromonedrive"

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
    __printf "  ${0} onedrive"
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
    "onedrive")
        __printf "rsyncfromonedrive onedrive" info

        cd ~/data/
        __rsync  ~/OneDrive/data/certs .
        __rsync  ~/OneDrive/data/secrets .
        __rsync  ~/OneDrive/data/ssh .
        __rsync  ~/OneDrive/data/vpn .

        cd ~/data/projects
        __rsync ~/OneDrive/data/projects/doc .
        __rsync ~/OneDrive/data/projects/ETK .

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
