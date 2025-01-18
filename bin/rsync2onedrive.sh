#!/bin/bash -
#===============================================================================
#
#          FILE: rsync2onedrive.sh
#
#         USAGE: ./rsync2onedrive.sh <target>
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: Ericsson Nikola Tesla d.d.
#       CREATED: 2021-08-27 08:32:35
#      REVISION: ---
#===============================================================================

## ----------------------------------------------------------------------------
## Variables
## ----------------------------------------------------------------------------

#### PATH
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### Banner
BANNER="rsync2onedrive"

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
        __printf "rsync2onedrive onedrive" info

        #### elx-5cg4126xwd: HP EliteBook 860 16 inch G10 Notebook PC/8B41, BIOS V70 Ver. 01.05.04 05/09/2024
        if [[ "$(hostname)" == "elx-5cg4126xwd" ]]
        then
            cd ~/data/
            __rsync certs        ~/OneDrive/data.ERICSSON/
            __rsync secrets      ~/OneDrive/data.ERICSSON/
            __rsync ssh          ~/OneDrive/data.ERICSSON/
            __rsync vpn          ~/OneDrive/data.ERICSSON/

            cd ~/data/projects
            __rsync doc          ~/OneDrive/data.ERICSSON/projects/
            __rsync ETK          ~/OneDrive/data.ERICSSON/projects/
        #### entd0001: HP EliteDesk 800 G2 SFF/8054, BIOS N01 Ver. 02.16 08/08/2016
        elif [[ "$(hostname)" == "entd0001" ]]
        then
            cd ~/data/
            __rsync certs        ~/OneDrive/data/
            __rsync secrets      ~/OneDrive/data/
            __rsync ssh          ~/OneDrive/data/
            __rsync vpn          ~/OneDrive/data/

            cd ~/data/projects
            __rsync doc          ~/OneDrive/data/projects/
            __rsync ETK          ~/OneDrive/data/projects/
        #### entl0002
        elif [[ "$(hostname)" == "entl0002" ]]
        then
            cd ~/data/
            __rsync certs        ~/OneDrive/data/
            __rsync secrets      ~/OneDrive/data/
            __rsync ssh          ~/OneDrive/data/
            __rsync vpn          ~/OneDrive/data/

            cd ~/data/projects
            __rsync doc          ~/OneDrive/data/projects/
            __rsync ETK          ~/OneDrive/data/projects/
        else
            cd ~/data/
            __rsync certs        ~/OneDrive/data/
            __rsync secrets      ~/OneDrive/data/
            __rsync ssh          ~/OneDrive/data/
            __rsync vpn          ~/OneDrive/data/

            cd ~/data/projects
            __rsync doc          ~/OneDrive/data/projects/
            __rsync ETK          ~/OneDrive/data/projects/
        fi

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
