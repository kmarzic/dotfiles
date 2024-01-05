#!/bin/bash -
#===============================================================================
#
#          FILE: rsyncntadmin2local.sh
#
#         USAGE: ./rsyncntadmin2local.sh
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: Ericsson Nikola Tesla d.d.
#       CREATED: 2021-12-11 08:32:35
#      REVISION: ---
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
    if [[ ! -d ~/data/projects/ETK/ICT/docs/ ]]
    then
        __printf "# mkdir -p ~/data/projects/ETK/ICT/doc" success
        mkdir -p ~/data/projects/ETK/ICT/docs
    fi

    if [[ ! -d ~/data/projects/ETK/ICT/docs/Servers/ ]]
    then
        __printf "# mkdir -p ~/data/projects/ETK/ICT/docs/Servers/" success
        mkdir -p ~/data/projects/ETK/ICT/docs/Servers/
    fi

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/ETK-PROJECT-s ~/data/projects/ETK/ICT/docs/" success
    rsync -avzhHp --delete /mnt/ntadmin/ETK-PROJECT-s ~/data/projects/ETK/ICT/docs/

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/arhiva.* ~/data/projects/ETK/ICT/docs/Servers/" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/arhiva.* ~/data/projects/ETK/ICT/docs/Servers/

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Servers_Team.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Servers_Team.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Landscape.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Landscape.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/LAB_ENT.HR.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/LAB_ENT.HR.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big-IP_servisi_*.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big-IP_servisi_*.xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big_IP_network*.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big_IP_network*.xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/RaspberryPI.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/RaspberryPI.xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/Servers-IPAddresses.backup.*.xls* ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/Servers-IPAddresses.backup.*.xls* ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.ericsson.hr.*.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.ericsson.hr.*xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.entlab.hr.*.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.entlab.hr.*xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/migration_plan.*.xlsx ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/migration_plan.*.xlsx ~/data/projects/ETK/ICT/docs/Servers

    __printf "# rsync -avzhHp --delete /mnt/ntadmin/Servers/IT-Migration ~/data/projects/ETK/ICT/docs/Servers" success
    rsync -avzhHp --delete /mnt/ntadmin/Servers/IT-Migration ~/data/projects/ETK/ICT/docs/Servers

    __printf ""
    __printf "# find ~/data/projects/ETK -type d -exec chmod 775 {} ';'" success
    __printf ~/data/projects/ETK -type d -exec chmod 775 {} ';'
    __printf "# find ~/data/projects/ETK -type f -exec chmod 644 {} ';'" success
    find ~/data/projects/ETK -type f -exec chmod 644 {} ';'
}

## ----------------------------------------------------------------------------
## MAIN
## ----------------------------------------------------------------------------

__banner
__rsync
__printf "Done!" success

exit ${EXIT_OK}

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
