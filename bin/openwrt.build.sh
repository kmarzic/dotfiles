#!/bin/bash
#===============================================================================
#
#          FILE: openwrt.build.sh
#
#         USAGE: ./openwrt.build.sh [ -h | -v <variant> | -p <platform> ]
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Kresimir Marzic (etkkrma), kresimir.marzic@ericsson.com
#  ORGANIZATION: MELA CU NCE ETK ICT DevOps IT Operations
#       CREATED: 2023-03-30
#      REVISION: ---
#===============================================================================

export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

#### Banner
BANNER="openwrt.build"

#### Debug
DEBUG=0 # debug is off
# DEBUG=1 # debug is on

## Exit
EXIT_OK=0
EXIT_ERROR=1


###############################################################################
## Functions
###############################################################################

## Function: Printf
##
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


## Function: Banner
##
function __banner()
{
    __printf "${BANNER}" info
}


## Function: Help
##
__help()
{
    __printf "Usage: ${0} [ -h | -v <variant> | -p <platform> ]"
    __printf "  -h                Help"
    __printf "  -v <variant>      Variant"
    __printf "  -p <platform>     Platform"
    __printf "  -l <file>  Log to <file>"
    __printf ""
    __printf "Variant:"
    __printf "   openwrt"
    __printf "   immortalwrt"
    __printf ""
    __printf "Examples:"
    __printf "   ${0} -p linksys_wrt3200acm                          -v openwrt"
    __printf "   ${0} -p mikrotik_routerboard-962uigs-5hact2hnt-ac   -v openwrt"
    __printf "   ${0} -p mikrotik_cap-ac                             -v openwrt"
    __printf "   ${0} -p asus_ax53u                                  -v openwrt"
    __printf "   ${0} -p friendlyarm_nanopi-r2s                      -v openwrt"
    __printf "   ${0} -p friendlyarm_nanopi-r4s                      -v openwrt"
    __printf "   ${0} -p friendlyarm_nanopi-r6s                      -v openwrt"
    __printf "   ${0} -p friendlyarm_nanopi-r6s                      -v immortalwrt"
    __printf "   ${0} -p generic                                     -v openwrt"
}

## Function: ImmortalWRT build
##
function __immortalwrt_build()
{
    __printf "ImmortalWRT build" info

    #### arg
    platform=${1}
    __printf "platform='${platform}'" debug

    #### set theme
    case ${platform} in
        #####################################################################
        "friendlyarm_nanopi-r6s")
        #####################################################################
            __printf "friendlyarm_nanopi-r6s" info

            #### (1)
            __printf "$ wget https://downloads.immortalwrt.org/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            wget https://downloads.immortalwrt.org/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            #### (2)
            # __printf "$ wget https://dl.wrt.moe/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            # wget https://dl.wrt.moe/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            #### (3)
            # __printf "$ wget https://immortalwrt.kyarucloud.moe/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            # wget https://immortalwrt.kyarucloud.moe/snapshots/targets/rockchip/armv8/immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            tar -I zstd -xf immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ cd immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64" success
            cd immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64

            __printf "$ sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config" success
            sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config

            __printf "$ sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config" success
            sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config

            # __printd "$ sed -i "s/downloads.immortalwrt.org/immortalwrt.kyarucloud.moe/g" repositories.conf" success
            # sed -i "s/downloads.immortalwrt.org/immortalwrt.kyarucloud.moe/g" repositories.conf

            __printf "$ make image PROFILE=friendlyarm_nanopi-r6s PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils \
e2fsprogs -automount -libustream-openssl\"" success
            make image PROFILE=friendlyarm_nanopi-r6s PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils \
e2fsprogs -automount -libustream-openssl"

            __printf "$ ls -la bin/targets/rockchip/armv8/" success
            ls -la bin/targets/rockchip/armv8/

            __printf "$ tar -cjf immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s" success
            scp immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s

            __printf "$ scp ../immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s" success
            scp ../immortalwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/rockchip/armv8/immortalwrt-rockchip-armv8-friendlyarm_nanopi-r6s-squashfs-sysupgrade.img.gz root@np2:/tmp" success
            scp -O bin/targets/rockchip/armv8/immortalwrt-rockchip-armv8-friendlyarm_nanopi-r6s-squashfs-sysupgrade.img.gz root@np2:/tmp
            ;;
    esac
}

## Function: OpenWRT build
##
function __openwrt_build()
{
    __printf "OpenWRT build" info

    #### arg
    platform=${1}
    __printf "platform='${platform}'" debug

    #### set theme
    case ${platform} in
        #####################################################################
        "linksys_wrt3200acm")
        #####################################################################
            __printf "linksys_wrt3200acm" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/mvebu/cortexa9/openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/mvebu/cortexa9/openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64/" success
            cd openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64/

            __printf "$ make image PROFILE=linksys_wrt3200acm PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full iwinfo \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wireless-tools wget-ssl xz-utils\"" success
            make image PROFILE=linksys_wrt3200acm PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full iwinfo \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wireless-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/mvebu/cortexa9/" success
            ls -la bin/targets/mvebu/cortexa9/

            __printf "$ tar -cjf openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mvebu_3200ACM" success
            scp openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mvebu_3200ACM

            __printf "$ scp ../openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mvebu_3200ACM" success
            scp ../openwrt-imagebuilder-mvebu-cortexa9.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mvebu_3200ACM

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/mvebu/cortexa9/openwrt-mvebu-cortexa9-linksys_wrt3200acm-squashfs-sysupgrade.bin root@10.254.200.1:/tmp" success
            scp -O bin/targets/mvebu/cortexa9/openwrt-mvebu-cortexa9-linksys_wrt3200acm-squashfs-sysupgrade.bin root@10.254.200.1:/tmp
            ;;
        #####################################################################
        "mikrotik_routerboard-962uigs-5hact2hnt-ac")
        #####################################################################
            __printf "mikrotik_routerboard-962uigs-5hact2hnt-ac" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/ath79/mikrotik/openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/ath79/mikrotik/openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64" success
            cd openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64

            __printf "$ make image PROFILE=mikrotik_routerboard-962uigs-5hact2hnt PACKAGES=\"\
adblock banip bzip2 curl dmesg dropbear \
ddns-scripts ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip ddns-scripts-services \
htop irqbalance ifstat iperf3 ipset \
luci luci-ssl luci-app-statistics luci-app-wifischedule luci-proto-wireguard \
netperf openssl-util openvpn-openssl tcpdump vim wireguard-tools\"" success
            make image PROFILE=mikrotik_routerboard-962uigs-5hact2hnt PACKAGES="\
adblock banip bzip2 curl dmesg dropbear \
ddns-scripts ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip ddns-scripts-services \
htop irqbalance ifstat iperf3 ipset \
luci luci-ssl luci-app-statistics luci-app-wifischedule luci-proto-wireguard \
netperf openssl-util openvpn-openssl tcpdump vim wireguard-tools"

            __printf "$ ls -la bin/targets/ath79/mikrotik/" success
            ls -la bin/targets/ath79/mikrotik/

            __printf "$ tar -cjf openwrt-imagebuilder-mikrotik_hap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-mikrotik_hap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-mikrotik_hap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mikrotik_hap_ac" success
            scp openwrt-imagebuilder-mikrotik_hap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mikrotik_hap_ac

            __printf "$ scp ../openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mikrotik_hap_ac" success
            scp ../openwrt-imagebuilder-ath79-mikrotik.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mikrotik_hap_ac

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/ath79/mikrotik/openwrt-ath79-mikrotik-mikrotik_routerboard-962uigs-5hact2hnt-squashfs-sysupgrade.bin root@10.254.205.4:/tmp" success
            scp -O bin/targets/ath79/mikrotik/openwrt-ath79-mikrotik-mikrotik_routerboard-962uigs-5hact2hnt-squashfs-sysupgrade.bin root@10.254.205.4:/tmp
            ;;
        #####################################################################
        "mikrotik_cap-ac")
        #####################################################################
            __printf "mikrotik_cap-ac" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/ipq40xx/mikrotik/openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/ipq40xx/mikrotik/openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64" success
            cd openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64/

            __printf "$ make image PROFILE=mikrotik_cap-ac PACKAGES=\"\
bzip2 curl dmesg dropbear gzip htop irqbalance ifstat iperf3 less lm-sensors  \
luci luci-ssl luci-app-advanced-reboot luci-app-statistics luci-app-wifischedule \
netperf openssl-util tcpdump vim vim-runtime wireless-tools wget-ssl xz-utils\"" success
            make image PROFILE=mikrotik_cap-ac PACKAGES="\
bzip2 curl dmesg dropbear gzip htop irqbalance ifstat iperf3 less lm-sensors  \
luci luci-ssl luci-app-advanced-reboot luci-app-statistics luci-app-wifischedule \
netperf openssl-util tcpdump vim vim-runtime wireless-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/ipq40xx/mikrotik/" success
            ls -la bin/targets/ipq40xx/mikrotik/

            __printf "$ tar -cjf openwrt-imagebuilder-mikrotik_cap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-mikrotik_cap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-mikrotik_cap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mikrotik_cap_ac" success
            scp openwrt-imagebuilder-mikrotik_cap-ac.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_mikrotik_cap_ac

            __printf "$ scp ../openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mikrotik_cap_ac" success
            scp ../openwrt-imagebuilder-ipq40xx-mikrotik.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_mikrotik_cap_ac

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/ipq40xx/mikrotik/openwrt-ipq40xx-mikrotik-mikrotik_cap-ac-squashfs-sysupgrade.bin root@capac.home.lan:/tmp" success
            scp -O bin/targets/ipq40xx/mikrotik/openwrt-ipq40xx-mikrotik-mikrotik_cap-ac-squashfs-sysupgrade.bin root@capac.home.lan:/tmp
            ;;
        #####################################################################
        "asus_ax53u")
        #####################################################################
            __printf "" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/ramips/mt7621/openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/ramips/mt7621/openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-ramips-mt7621.Linux-x86_64" success
            cd openwrt-imagebuilder-ramips-mt7621.Linux-x86_64

            __printf "$ make image PROFILE=asus_rt-ax53u PACKAGES=\"\
bzip2 curl dmesg dropbear gzip htop irqbalance ifstat iperf3 less lm-sensors  \
luci luci-ssl luci-app-advanced-reboot luci-app-statistics luci-app-wifischedule \
netperf openssl-util tcpdump vim vim-runtime wireless-tools wget-ssl xz-utils\"" success
            make image PROFILE=asus_rt-ax53u PACKAGES="\
bzip2 curl dmesg dropbear gzip htop irqbalance ifstat iperf3 less lm-sensors  \
luci luci-ssl luci-app-advanced-reboot luci-app-statistics luci-app-wifischedule \
netperf openssl-util tcpdump vim vim-runtime wireless-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/ramips/mt7621/" success
            ls -la bin/targets/ramips/mt7621/

            __printf "$ tar -cjf openwrt-imagebuilder-asus-ax53u.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-asus-ax53u.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-asus-ax53u.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_asus_ax53u" success
            scp openwrt-imagebuilder-asus-ax53u.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_asus_ax53u

            __printf "$ scp ../openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_asus_ax53u" success
            scp ../openwrt-imagebuilder-ramips-mt7621.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_asus_ax53u

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/ramips/mt7621/openwrt-ramips-mt7621-asus_rt-ax53u-squashfs-sysupgrade.bin root@ax53u.home.lan:/tmp" success
            scp -O bin/targets/ramips/mt7621/openwrt-ramips-mt7621-asus_rt-ax53u-squashfs-sysupgrade.bin root@ax53u.home.lan:/tmp
            ;;
        #####################################################################
        "friendlyarm_nanopi-r2s")
        #####################################################################
            __printf "friendlyarm_nanopi-r2s" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/" success
            cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/

            __printf "$ sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=256/g" .config" success
            sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=256/g" .config

            __printf "$ sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config" success
            sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config

            __printf "$ make image PROFILE=friendlyarm_nanopi-r2s PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils\"" success
            make image PROFILE=friendlyarm_nanopi-r2s PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/rockchip/armv8/" success
            ls -la bin/targets/rockchip/armv8/

            __printf "$ tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r2s" success
            scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r2s

            __printf "$ scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r2s" success
            scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r2s

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r2s-squashfs-sysupgrade.img.gz root@10.254.200.1:/tmp" success
            scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r2s-squashfs-sysupgrade.img.gz root@10.254.200.1:/tmp
            ;;
        #####################################################################
        "friendlyarm_nanopi-r4s")
        #####################################################################
            __printf "friendlyarm_nanopi-r4s" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/" success
            cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/

            __printf "$ sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config" success
            sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config

            __printf "$ sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config" success
            sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config

            __printf "$ make image PROFILE=friendlyarm_nanopi-r4s PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils\"" success
            make image PROFILE=friendlyarm_nanopi-r4s PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/rockchip/armv8/" success
            ls -la bin/targets/rockchip/armv8/

            __printf "$ tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r4s" success
            scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r4s

            __printf "$ scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r4s" success
            scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r4s

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r4s-squashfs-sysupgrade.img.gz root@np1:/tmp" success
            scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r4s-squashfs-sysupgrade.img.gz root@np1:/tmp
            ;;
        #####################################################################
        "friendlyarm_nanopi-r6s")
        #####################################################################
            __printf "friendlyarm_nanopi-r6s" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/rockchip/armv8/openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/" success
            cd openwrt-imagebuilder-rockchip-armv8.Linux-x86_64/

            __printf "$ sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config" success
            sed -i "s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config

            __printf "$ sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config" success
            sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE=.*/CONFIG_TARGET_KERNEL_PARTSIZE=32/g" .config

            __printf "$ make image PROFILE=friendlyarm_nanopi-r6s PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils\"" success
            make image PROFILE=friendlyarm_nanopi-r6s PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils"

            __printf "$ ls -la bin/targets/rockchip/armv8/" success
            ls -la bin/targets/rockchip/armv8/

            __printf "$ tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s" success
            scp openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s

            __printf "$ scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s" success
            scp ../openwrt-imagebuilder-rockchip-armv8.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_rockchip_r6s

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r6s-squashfs-sysupgrade.img.gz root@np2:/tmp" success
            scp -O bin/targets/rockchip/armv8/openwrt-rockchip-armv8-friendlyarm_nanopi-r6s-squashfs-sysupgrade.img.gz root@np2:/tmp
            ;;
        #####################################################################
        "generic")
        #####################################################################
            __printf "generic" info

            __printf "$ wget https://downloads.openwrt.org/snapshots/targets/x86/64/openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst" success
            wget https://downloads.openwrt.org/snapshots/targets/x86/64/openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst

            __printf "$ tar -I zstd -xf openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst" success
            tar -I zstd -xf openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst

            __printf "$ cd openwrt-imagebuilder-x86-64.Linux-x86_64" success
            cd openwrt-imagebuilder-x86-64.Linux-x86_64

            __printf "$ sed -i \"s/CONFIG_TARGET_KERNEL_PARTSIZE.*/CONFIG_TARGET_KERNEL_PARTSIZE=128/g;s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g\" .config" success
            sed -i "s/CONFIG_TARGET_KERNEL_PARTSIZE.*/CONFIG_TARGET_KERNEL_PARTSIZE=128/g;s/CONFIG_TARGET_ROOTFS_PARTSIZE=.*/CONFIG_TARGET_ROOTFS_PARTSIZE=1024/g" .config

            __printf "$ make image PROFILE=generic PACKAGES=\"\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils \
intel-microcode fdisk f2fs-tools fstrim kmod-leds-gpio kmod-crypto-hw-ccp kmod-usb-ohci kmod-pcspkr losetup resize2fs\"" success
            make image PROFILE=generic PACKAGES="\
6in4 adblock banip block-mount bridge bzip2 comgt curl ddns-scripts-cloudflare ddns-scripts-freedns ddns-scripts-noip \
dnscrypt-proxy2 dmesg dropbear e2fsprogs gzip htop ifstat iperf3 ip-bridge ip-full \
kmod-fs-autofs4 kmod-fs-ext4 kmod-fs-msdos kmod-fs-ntfs kmod-tun kmod-usb-storage-uas kmod-usb2 kmod-usb3 \
lm-sensors ncat nmap nping less liblzo2 \
luci luci-ssl luci-app-adblock luci-app-advanced-reboot luci-app-banip luci-app-bcp38 luci-app-ddns luci-app-openvpn luci-app-sqm \
luci-app-statistics luci-app-vnstat2 luci-proto-wireguard \
mkf2fs mailsend netdata netperf ntfs-3g openvpn-openssl openssl-util siproxd sqm-scripts stubby \
tcpdump unrar unzip vim vim-runtime vnstat2 wireguard-tools wget-ssl xz-utils \
intel-microcode fdisk f2fs-tools fstrim kmod-leds-gpio kmod-crypto-hw-ccp kmod-usb-ohci kmod-pcspkr losetup resize2fs"

            __printf "$ ls -la bin/targets/x86/64/" success
            ls -la bin/targets/x86/64/

            __printf "$ tar -cjf openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config" success
            tar -cjf openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 bin/ dl/ .config

            __printf "proceed with copy to 'scully' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_x86" success
            scp openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 kmarzic@scully.lan:/data/media/openwrt_x86

            __printf "$ scp ../openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_x86" success
            scp ../openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst kmarzic@scully.lan:/data/media/openwrt_x86

            __printf "proceed with copy to 'OpenWRT' (y/n):" success
            read input1
            [[ ${input1} == "n" ]] && __printf "exit..." && exit ${EXIT_ERROR}
            [[ ${input1} == "y" ]] && __printf "continue..."

            __printf "$ scp -O bin/targets/x86/64/openwrt-x86-64-generic-ext4-combined-efi.img.gz root@10.254.200.1:/tmp" success
            scp -O bin/targets/x86/64/openwrt-x86-64-generic-ext4-combined-efi.img.gz root@10.254.200.1:/data/temp

            __printf "$ scp openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 root@10.254.200.1:/data/openwrt_x86" success
            scp openwrt-imagebuilder-x86-64.Linux-x86_64.x86.$(date "+%Y%m%d").tar.bz2 root@10.254.200.1:/data/openwrt_x86

            __printf "$ scp ../openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst root@10.254.200.1:/data/openwrt_x86" success
            scp ../openwrt-imagebuilder-x86-64.Linux-x86_64.tar.zst root@10.254.200.1:/data/openwrt_x86
            ;;
    esac
}


###############################################################################
## Main
###############################################################################

#### Banner
__banner

#### Command Line
__platform_flag=""

while getopts "hv:p:l:" opt;
do
    case ${opt} in
        h)
            __help
            exit ${EXIT_OK}
            ;;
        v)
            __platform_flag="true"
            VARIANT=${OPTARG}
            ;;
        p)
            __platform_flag="true"
            PLATFORM=${OPTARG}
            ;;
        l)
            LOG_ENABLED="true"
            LOG_FILE=${OPTARG}
            __printf "enabling logging to '${LOG_FILE}' ..." info
            ;;
        :)
            __printf "Option -${opt} requires an argument!" error
            exit ${EXIT_ERROR}
            ;;
    esac
done

#### This tells getopts to move on to the next argument.
shift $((OPTIND-1))

if [[ -z "${__platform_flag}" ]]
then
    __printf "Missing arguments!" error
    __help
    exit ${EXIT_ERROR}
else
    [[ "${VARIANT}" == "immortalwrt" ]] && __immortalwrt_build "${PLATFORM}"
    [[ "${VARIANT}" == "openwrt" ]]     && __openwrt_build "${PLATFORM}"
fi

#### Done
__printf "done!"

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
