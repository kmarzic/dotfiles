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

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

echo "rsyncntadmin2local"

## ----------------------------------------------------------------------------
## MAIN
## ----------------------------------------------------------------------------

if [[ ! -d ~/data/projects/ETK/ICT/docs/ ]]
then
   echo "# mkdir -p ~/data/projects/ETK/ICT/doc"
   mkdir -p ~/data/projects/ETK/ICT/docs
fi

if [[ ! -d ~/data/projects/ETK/ICT/docs/Servers/ ]]
then
   echo "# mkdir -p ~/data/projects/ETK/ICT/docs/Servers/"
   mkdir -p ~/data/projects/ETK/ICT/docs/Servers/
fi

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/ETK-PROJECT-s ~/data/projects/ETK/ICT/docs/"
rsync -avzhHp --delete /mnt/ntadmin/ETK-PROJECT-s ~/data/projects/ETK/ICT/docs/

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/arhiva.* ~/data/projects/ETK/ICT/docs/Servers/"
rsync -avzhHp --delete /mnt/ntadmin/Servers/arhiva.* ~/data/projects/ETK/ICT/docs/Servers/

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Servers_Team.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Servers_Team.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Landscape.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/ETK_WO_EIO_Landscape.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/LAB_ENT.HR.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/LAB_ENT.HR.READ_ONLY.* ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big-IP_servisi_*.xlsx ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big-IP_servisi_*.xlsx ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big_IP_network*.xlsx ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/F5_Big_IP_network*.xlsx ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/RaspberryPI.xlsx ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/RaspberryPI.xlsx ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/Servers-IPAddresses.backup.*.xls* ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/Servers-IPAddresses.backup.*.xls* ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.ericsson.hr.*.xlsx ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/mail.ericsson.hr.*xlsx ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/migration_plan.*.xlsx ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/migration_plan.*.xlsx ~/data/projects/ETK/ICT/docs/Servers

echo "# rsync -avzhHp --delete /mnt/ntadmin/Servers/IT-Migration ~/data/projects/ETK/ICT/docs/Servers"
rsync -avzhHp --delete /mnt/ntadmin/Servers/IT-Migration ~/data/projects/ETK/ICT/docs/Servers

echo ""
echo "# find ~/data/projects/ETK -type d -exec chmod 775 {} ';'"
find ~/data/projects/ETK -type d -exec chmod 775 {} ';'
echo "# find ~/data/projects/ETK -type f -exec chmod 644 {} ';'"
find ~/data/projects/ETK -type f -exec chmod 644 {} ';'

## ----------------------------------------------------------------------------
## end
## ----------------------------------------------------------------------------
