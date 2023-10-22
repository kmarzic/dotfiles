#!/bin/bash - 
#===============================================================================
#
#          FILE: rsync2usb.sh
# 
#         USAGE: ./rsync2usb.sh <target>
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: Ericsson Nikola Tesla d.d.
#       CREATED: 2015-07-20 09:34:33
#      REVISION:  ---
#===============================================================================

## ----------------------------------------------------------------------------
## Variables
## ----------------------------------------------------------------------------

#### PATH
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### Banner
BANNER="rsyncadmin2local"

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
    if [ -z ${1} ]
    then
        DEV="/media/usb.ntfs1"
    else
        DEV="${1}"
    fi

    echo "DEV=${DEV}"

    __printf "# rsync -rltDvu --progress --delete ~/data/projects/* ${DEV}/data/projects" success
    rsync -rltDvu --progress --delete ~/data/projects/* ${DEV}/data/projects

    __printf "# rsync -rltDvu --progress --delete ~/data/ssh/* ${DEV}/data/ssh" success
    rsync -rltDvu --progress --delete ~/data/ssh/* ${DEV}/data/ssh

    __printf "# rsync -rltDvu --progress --delete ~/data/certs/* ${DEV}/data/certs" success
    rsync -rltDvu --progress --delete ~/data/certs/* ${DEV}/data/certs

    __printf "# rsync -rltDvu --progress --delete ~/data/vpn/* ${DEV}/data/vpns" success
    rsync -rltDvu --progress --delete ~/data/vpn/* ${DEV}/data/vpn

    __printf "# rsync -rltDvu --progress --delete ~/data/secrets/* ${DEV}/data/secrets" success
    rsync -rltDvu --progress --delete ~/data/secrets/* ${DEV}/data/secrets
}

## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------

__banner
__rsync
__printf "Done!" success

exit ${EXIT_OK}

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
