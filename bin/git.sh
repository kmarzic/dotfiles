#!/bin/bash

## ----------------------------------------------------------------------------
## Variables
## ----------------------------------------------------------------------------

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin

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


## ----------------------------------------------------------------------------
## Functions
## ----------------------------------------------------------------------------

function __delete_old_archives()
{
    cd ${DIR}
    echo "# rm -f *.bz2 *.gz"
    rm -f *.bz2 *.gz
}


function __git_update()
{
    echo "--------------------------------------------------------------------------------"
    arg1=${1} # repo
    echo "arg1=${arg1}"
    arg2=${2} # remote
    echo "arg2=${arg2}"

    ## Git Clone
    if test -e "${DIR}/${arg1}"
    then
        echo "Directory '${DIR}/${arg1}' found!"
        cd ${DIR}
        cd ${arg1}
    else
        echo "Directory '${DIR}/${arg1}' NOT found!"
        cd ${DIR}
        echo "# git clone ${GIT_LOCAL}/${arg1}"
        git clone ${GIT_LOCAL}/${arg1}
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
            echo "repo='${repo}' remote='${remote}' url='${url}'"
            remote_status=$(git remote | grep ${remote} | wc -l)

            if [ ${remote_status} -eq 0 ]
            then
                echo "# git remote add ${remote} ${url}"
                git remote add ${remote} ${url}
            fi
        fi
    done

    ## Origin
    echo "# git checkout master"
    git checkout master

    echo "# git fetch origin master"
    git fetch origin master

    echo "# git log master..origin/master"
    git log master..origin/master

    echo "# git pull origin master"
    git pull origin master

    ## Push origin
    echo "# git push origin master"
    git push origin master

    ## Git local branches
    for branch in $(git branch -a | egrep -v "remotes|origin|HEAD|master");
    do
        echo "branch='${branch}'"

        echo "# git branch -d ${branch}"
        git branch -d ${branch}

        echo "# git fetch origin ${branch}"
        git fetch origin ${branch}

        echo "# git checkout -b ${branch}"
        git checkout -b ${branch}

        echo "# git log master..${branch}"
        git log master..${branch}

        echo "# git pull origin ${branch}"
        git pull origin ${branch}

        # echo "# git push origin ${branch}"
        # git push origin ${branch}

        echo "# git checkout master"
        git checkout master
    done

    ## Get branches list
    echo "# git remote update --prune origin"
    git remote update --prune origin

    ## Remote
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')

        if [[ "${arg1}" = "${repo}" ]]
        then
            echo "repo='${repo}' remote='${remote}' url='${url}'"
            remote_status=$(git remote | grep ${remote} | wc -l)

            if [ "${arg2}" = "${remote}" ]
            then
                echo "# git remote update --prune ${remote}"
                git remote update --prune ${remote}

                echo "# git fetch ${remote}"
                git fetch ${remote}

                echo "# git log master..${remote}/master"
                git log master..${remote}/master

                echo "# git pull ${remote} master --stat"
                git pull ${remote} master --stat

                echo "# git push origin master"
                git push origin master

                for remote in $(git branch -r | grep ${remote} | grep -v "master");
                do
                    echo ""
                    branch=$(echo ${remote} | awk -F '/' '{print $2}')

                    echo "# git branch -d ${branch}"
                    # git branch -d ${branch}

                    echo "# git fetch ${service_key} ${branch}"
                    # git fetch ${service_key} ${branch}

                    echo "# git checkout -b ${branch}"
                    # git checkout -b ${branch}

                    echo "# git log master..${branch}"
                    # git log master..${branch}

                    echo "# git pull ${service_key} ${branch}"
                    # git pull ${service_key} ${branch}

                    echo "# git push origin ${branch}"
                    # git push origin ${branch}

                    echo "# git checkout master"
                    # git checkout master
                done
            fi
        fi
    done

    cd ..
    echo "--------------------------------------------------------------------------------"
}


function __git_backup()
{
    arg1=${1} # repo
    echo "arg1=${arg1}"
    arg2=${2} # remote
    echo "arg2=${arg2}"

    if test -e "${DIR}/${arg1}"
    then
        echo "--------------------------------------------------------------------------------"
        echo "Directory '${DIR}/${arg1}' found!"
        cd ${DIR}/${arg1}
        echo "# git bundle create ${DIR}/${arg1}-${DATE}.git_bundle --all"
        git bundle create ${DIR}/${arg1}-${DATE}.git_bundle --all
        echo "# bzip2 -9 ${DIR}/${arg1}-${DATE}.git_bundle"
        bzip2 -9 ${DIR}/${arg1}-${DATE}.git_bundle
        cd -
        echo "--------------------------------------------------------------------------------"
    else
        echo "--------------------------------------------------------------------------------"
        echo "Directory '${DIR}/${arg1}' NOT found!"
        echo "--------------------------------------------------------------------------------"
    fi
}


function __git_remotes()
{
    for service in "${GIT_REMOTE[@]}";
    do
        repo=$(echo ${service} | awk -F ';' '{ print $1 }')
        remote=$(echo ${service} | awk -F ';' '{ print $2 }')
        url=$(echo ${service} | awk -F ';' '{ print $3 }')
        echo "repo='${repo}' remote='${remote}' url='${url}'"
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
        [[ "${arg}" = "${repo}" ]] && echo "repo='${repo}' remote='${remote}' url='${url}'"
    done
}


## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------

echo "Git update and backup"

case "${1}" in
    update)
        echo "GIT Update"
        __git_remotes
        __git_update arduino ${2}
        __git_update docs ${2}
        __git_update dotfiles ${2}
        __git_update ETK ${2}
        __git_update homeserver ${2}
        __git_update openwrt ${2}
        __git_update sources ${2}
        echo "Update done!"
        ;;
    backup)
        echo "GIT Backup"
        __delete_old_archives
        __git_backup arduino
        __git_backup docs
        __git_backup dotfiles
        __git_backup ETK
        __git_backup homeserver
        __git_backup openwrt
        __git_backup sources
        echo "Backup done!"
        ;;
    test)
        __test "docs"
        ;;
    *)
        echo "Usage:"
        echo "  ${0} {update|backup} <server>"

        echo "Server:"
        echo "  gmn, scully"

        echo "Repos:"
        for service in "${GIT_REMOTE[@]}";
        do
            repo=$(echo ${service} | awk -F ';' '{ print $1 }')
            remote=$(echo ${service} | awk -F ';' '{ print $2 }')
            url=$(echo ${service} | awk -F ';' '{ print $3 }')
            echo "  repo='${repo}' remote='${remote}' url='${url}'"
        done

        echo "Example:"
        echo "  ${0} update gmn"
        echo "  ${0} update scully"
        echo "  ${0} backup"
        ;;
esac

cd ${PWD}

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
