#!/bin/bash

## ----------------------------------------------------------------------------
## Variables
## ----------------------------------------------------------------------------

#### PATH
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin

#### Banner
BANNER="Git update and backup"

DIR="${HOME}/git/home"
PWD=$(pwd)
DATE=$(date +%Y%m%d_%H%M)
HOST=$(hostname)
USER=$(id | cut -d"(" -f2 | cut -d ")" -f1)

GIT_LOCAL="git+ssh://${USER}@${HOST}/srv/git"

GIT_REMOTE=(
"arduino;gmn;git@gitea.lan:home/arduino.git"
"arduino;scully;git+ssh://kmarzic@scully/srv/git/arduino"
"docs;gmn;git@gitea.lan:kmarzic/docs.git"
"docs;scully;git+ssh://kmarzic@scully/srv/git/docs"
"dotfiles;gmn;git@gitea.lan:kmarzic/dotfiles.git"
"dotfiles;scully;git+ssh://kmarzic@scully/srv/git/dotfiles"
"ETK;gmn;git@gitea.lan:kmarzic/ETK.git"
"ETK;scully;git+ssh://kmarzic@scully/srv/git/ETK"
"homeserver;gmn;git@gitea.lan:home/homeserver.git"
"homeserver;scully;git+ssh://kmarzic@localhost/srv/git/homeserver"
"openwrt;gmn;git@gitea.lan:home/openwrt.git"
"openwrt;scully;git+ssh://kmarzic@scully/srv/git/openwrt"
"sources;gmn;git@gitea.lan:kmarzic/sources.git"
"sources;scully;git+ssh://kmarzic@scully/srv/git/sources"
)

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


function __help()
{
    __printf "Usage:"
    __printf "  ${0} {update|backup} <server>"

    __printf "Server:"
    __printf "  gmn, scully"

    __printf "Repos:"
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')
        __printf "  repo='${repo}' remote='${remote}' url='${url}'"
    done

    __printf "Example:"
    __printf "  ${0} update gmn"
    __printf "  ${0} update scully"
    __printf "  ${0} backup"
}


function __delete_old_archives()
{
    __printf "# cd ${DIR}" success
    cd ${DIR}

    __printf "# rm -f *.bz2 *.gz" success
    rm -f *.bz2 *.gz
}


function __git_update()
{
    __printf "--------------------------------------------------------------------------------"
    arg1=${1} # repo
    __printf "arg1=${arg1}"
    arg2=${2} # remote
    __printf "arg2=${arg2}"

    ## Git Clone
    if test -e "${DIR}/${arg1}"
    then
        __printf "Directory '${DIR}/${arg1}' found!" success

        __printf "# cd ${DIR}" success
        cd ${DIR}

        __printf "# cd ${arg1}" success
        cd ${arg1}
    else
        __printf "Directory '${DIR}/${arg1}' NOT found!" success

        __printf "# cd ${DIR}" success
        cd ${DIR}

        __printf "# git clone ${GIT_LOCAL}/${arg1}" success
        git clone ${GIT_LOCAL}/${arg1}

        __printf "# cd ${arg1}" success
        cd ${arg1}
    fi

    ## Git add remote
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')

        if [[ "${arg1}" = "${repo}" ]]
        then
            __printf "repo='${repo}' remote='${remote}' url='${url}'" info
            remote_status=$(git remote | grep ${remote} | wc -l)

            if [ ${remote_status} -eq 0 ]
            then
                __printf "# git remote add ${remote} ${url}" success
                git remote add ${remote} ${url}
            fi
        fi
    done

    ## Origin
    __printf "# git checkout master" success
    git checkout master

    __printf "# git fetch origin master" success
    git fetch origin master

    __printf "# git log master..origin/master" success
    git log master..origin/master

    __printf "# git pull origin master" success
    git pull origin master

    ## Push origin
    __printf "# git push origin master" success
    git push origin master

    ## Git local branches
    for branch in $(git branch -a | egrep -v "remotes|origin|HEAD|master");
    do
        __printf "branch='${branch}'" info

        __printf "# git branch -d ${branch}" success
        git branch -d ${branch}

        __printf "# git fetch origin ${branch}" success
        git fetch origin ${branch}

        __printf "# git checkout -b ${branch}" success
        git checkout -b ${branch}

        __printf "# git log master..${branch}" success
        git log master..${branch}

        __printf "# git pull origin ${branch}" success
        git pull origin ${branch}

        # __printf "# git push origin ${branch}" success
        # git push origin ${branch}

        __printf "# git checkout master" success
        git checkout master
    done

    ## Get branches list
    __printf "# git remote update --prune origin" success
    git remote update --prune origin

    ## Remote
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')

        if [[ "${arg1}" = "${repo}" ]]
        then
            __printf "repo='${repo}' remote='${remote}' url='${url}'" info
            remote_status=$(git remote | grep ${remote} | wc -l)

            if [ "${arg2}" = "${remote}" ]
            then
                __printf "# git remote update --prune ${remote}" success
                git remote update --prune ${remote}

                __printf "# git fetch ${remote}" success
                git fetch ${remote}

                __printf "# git log master..${remote}/master" success
                git log master..${remote}/master

                __printf "# git pull ${remote} master --stat" success
                git pull ${remote} master --stat

                __printf "# git push origin master" success
                git push origin master

                for remote in $(git branch -r | grep ${remote} | grep -v "master");
                do
                    __printf ""
                    branch=$(echo ${remote} | awk -F '/' '{print $2}')
                    __printf "branch='${branch}'" info

                    __printf "# git branch -d ${branch}" success
                    # git branch -d ${branch}

                    __printf "# git fetch ${service_key} ${branch}" success
                    # git fetch ${service_key} ${branch}

                    __printf "# git checkout -b ${branch}" success
                    # git checkout -b ${branch}

                    __printf "# git log master..${branch}" success
                    # git log master..${branch}

                    __printf "# git pull ${service_key} ${branch}" success
                    # git pull ${service_key} ${branch}

                    __printf "# git push origin ${branch}" success
                    # git push origin ${branch}

                    __printf "# git checkout master" success
                    # git checkout master
                done
            fi
        fi
    done

    cd ..
    __printf "--------------------------------------------------------------------------------"
}


function __git_backup()
{
    arg1=${1} # repo
    echo "arg1=${arg1}"
    arg2=${2} # remote
    echo "arg2=${arg2}"

    if test -e "${DIR}/${arg1}"
    then
        __printf "--------------------------------------------------------------------------------"
        __printf "Directory '${DIR}/${arg1}' found!" success

        __printf "# cd ${DIR}/${arg1}" success
        cd ${DIR}/${arg1}

        __printf "# git bundle create ${DIR}/${arg1}-${DATE}.git_bundle --all" success
        git bundle create ${DIR}/${arg1}-${DATE}.git_bundle --all

        __printf "# bzip2 -9 ${DIR}/${arg1}-${DATE}.git_bundle" success
        bzip2 -9 ${DIR}/${arg1}-${DATE}.git_bundle

        __printf "# cd -" success
        cd -
        __printf "--------------------------------------------------------------------------------"
    else
        __printf "--------------------------------------------------------------------------------"
        __printf "Directory '${DIR}/${arg1}' NOT found!" error
        __printf "--------------------------------------------------------------------------------"
    fi
}


function __git_remotes()
{
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')
        __printf "repo='${repo}' remote='${remote}' url='${url}'" info
    done
}


function __test()
{
    arg=${1}
    echo "arg=${arg}"

    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')
        [[ "${arg}" = "${repo}" ]] && __printf "repo='${repo}' remote='${remote}' url='${url}'" info
    done
}


## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------

__banner

case "${1}" in
    update)
        __printf "GIT Update" info

        __git_remotes
        __git_update arduino ${2}
        __git_update docs ${2}
        __git_update dotfiles ${2}
        __git_update ETK ${2}
        __git_update homeserver ${2}
        __git_update openwrt ${2}
        __git_update sources ${2}

        __printf "# cd ${PWD}" success
        cd ${PWD}

        __printf "Update done!" success
        ;;
    backup)
        __printf "GIT Backup" info

        __delete_old_archives
        __git_backup arduino
        __git_backup docs
        __git_backup dotfiles
        __git_backup ETK
        __git_backup homeserver
        __git_backup openwrt
        __git_backup sources

        __printf "# cd ${PWD}" success
        cd ${PWD}

        __printf "Backup done!" success
        ;;
    test)
        __test "docs"
        ;;
    *)
        __help
        ;;
esac

exit ${EXIT_OK}

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
