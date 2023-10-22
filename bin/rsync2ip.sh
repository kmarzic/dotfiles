#!/bin/bash - 
#===============================================================================
#
#          FILE: rsync2ip.sh
# 
#         USAGE: ./rsync2ip.sh
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

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

function __rsync()
{
    dir=${1}
    destination=${2}

    echo ""
    echo "# rsync -rltDvu --progress --delete ${dir} ${destination}"
    rsync -rltDvu --progress --delete ${dir} ${destination}
}

echo "rsync2іp"
echo "usage:"
echo   "${0} target"
echo "example:"
echo   "${0} admkremar@etk.pckrma"

target=${1}

if [ -z ${target} ]
then
    echo "target not defined!"
    exit 1;
fi

# rsync -rltDvu --progress --delete ~/data/projects/* ${target}:/home/admkremar/data/projects
cd ~/data
__rsync projects          ${target}:/home/admkremar/data
__rsync ssh               ${target}:/home/admkremar/data
__rsync certs             ${target}:/home/admkremar/data
__rsync vpn               ${target}:/home/admkremar/data
__rsync secrets           ${target}:/home/admkremar/data

cd ~/
__rsync wallpapers        ${target}:/home/admkremar
__rsync bin               ${target}:/home/admkremar
__rsync Maildir           ${target}:/home/admkremar

## end
