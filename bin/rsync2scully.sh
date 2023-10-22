#!/bin/bash - 
#===============================================================================
#
#          FILE: rsync2scully.sh
#
#         USAGE: ./rsync2scully.sh
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

echo "rsync2scully"

rsync -rltDvu --progress --delete ~/data/projects/* kmarzic@scully:/data/projects
rsync -rltDvu --progress --delete ~/wallpapers/*    kmarzic@scully:/data/photo/wallpapers

## end
