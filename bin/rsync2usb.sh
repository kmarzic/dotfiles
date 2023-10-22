#!/bin/bash - 
#===============================================================================
#
#          FILE: rsync2usb.sh
# 
#         USAGE: ./rsync2usb.sh 
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

# set -o nounset                              # Treat unset variables as an error

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

if [ -z ${1} ]
then
    DEV="/media/usb.ntfs1"
else
    DEV="${1}"
fi

echo "DEV=${DEV}"

rsync -rltDvu --progress --delete ~/data/projects/* ${DEV}/projects

## end
