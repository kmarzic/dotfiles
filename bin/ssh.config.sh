#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin
SSH_DIR="${HOME}/data/ssh"
GIT_CONNECT="${HOME}/git/home/ETK/trunk/ICT/connect"

echo "Compile ~/.ssh/config"

case "$1" in
'ericsson' | 'internet' | 'home')
    echo "Compile ~/.ssh/config ${1}"
    #
    cat ${SSH_DIR}/config.default > ~/.ssh/config
    #
    cat ${SSH_DIR}/config.a1_iptv >> ~/.ssh/config
    cat ${SSH_DIR}/config.ehkz.dev >> ~/.ssh/config
    cat ${SSH_DIR}/config.ehkz.prod >> ~/.ssh/config
    cat ${SSH_DIR}/config.kbcsplit >> ~/.ssh/config
    cat ${SSH_DIR}/config.kib.fran.mihaljevic >> ~/.ssh/config
    cat ${SSH_DIR}/config.sb.makarska >> ~/.ssh/config
    cat ${SSH_DIR}/config.zup.test >> ~/.ssh/config
    cat ${SSH_DIR}/config.fly >> ~/.ssh/config
    cat ${SSH_DIR}/config.vbox >> ~/.ssh/config
    #
    cat ${SSH_DIR}/config.ericsson.ecn >> ~/.ssh/config
    cat ${SSH_DIR}/config.ericsson.etk >> ~/.ssh/config
    cat ${SSH_DIR}/config.ericsson.lab >> ~/.ssh/config
    #
    cat ${SSH_DIR}/config.home.${1} >> ~/.ssh/config
    #
    chmod 600 ~/.ssh/config
    chmod 600 ${SSH_DIR}/id_rsa*
    ;;
'gen')
    echo "Generate"

    if [[ -d ${GIT_CONNECT} ]]
    then
        oldifs="${IFS}"
        IFS=$'\n'

        for line in $(cat ${GIT_CONNECT}/f5.txt ${GIT_CONNECT}/infoblox.txt ${GIT_CONNECT}/servers.*.txt)
        do
            ip=$(echo ${line} | awk -F ';' '{ print $1 }')
            host=$(echo ${line} | awk -F ';' '{ print $2 }')

            [[ -z ${host} ]] && continue

            case "${host}" in
            *)
                echo "## ${line}"
                echo "Host etk.${host}"
                echo "    Hostname ${ip}"
                echo "    User etkadmin"
                echo "    IdentityFile ~/data/ssh/id_rsa.etk.it.etkadmin"
                if [ "${host}" == "ehrzgva486" ]  || [ "${host}" == "ehrzgittebck" ] || [ "${host}" == "ehrzgux211" ]   ||
                   [ "${host}" == "ehrzglx2169" ] || [ "${host}" == "exzglx050" ]    || [ "${host}" == "utra1" ]        ||
                   [ "${host}" == "zisovmm" ]
                then
                    echo "    PubkeyAcceptedKeyTypes=ssh-rsa"
                    echo "    HostKeyAlgorithms=ssh-rsa"
                    echo "    KexAlgorithms=diffie-hellman-group1-sha1"
                    echo "    Ciphers=aes256-cbc,3des-cbc"
                    echo "    MACs=hmac-md5,hmac-sha2-512"
                fi
                if [ "${host}" == "exzglxbybackup" ] || [ "${host}" == "filenet01" ] || [ "${host}" == "supportclone" ] ||
                   [ "${host}" == "ehrzgux468" ]
                then
                    echo "    PubkeyAcceptedKeyTypes=ssh-rsa"
                fi
                echo ""

                echo "Host etk.ansible.${host}"
                echo "    Hostname ${ip}"
                echo "    User etkadmin"
                echo "    IdentityFile ~/data/ssh/id_rsa.etk.it.etkadmin"
                if [ "${host}" == "ehrzgva486" ]  || [ "${host}" == "ehrzgittebck" ] || [ "${host}" == "ehrzgux211" ]   ||
                   [ "${host}" == "ehrzglx2169" ] || [ "${host}" == "exzglx050" ]    || [ "${host}" == "utra1" ]        ||
                   [ "${host}" == "zisovmm" ]
                then
                    echo "    PubkeyAcceptedKeyTypes=ssh-rsa"
                    echo "    HostKeyAlgorithms=ssh-rsa"
                    echo "    KexAlgorithms=diffie-hellman-group1-sha1"
                    echo "    Ciphers=aes256-cbc,3des-cbc"
                    echo "    MACs=hmac-md5,hmac-sha2-512"
                fi
                if [ "${host}" == "exzglxbybackup" ] || [ "${host}" == "filenet01" ] || [ "${host}" == "supportclone" ] ||
                   [ "${host}" == "ehrzgux468" ]
                then
                    echo "    PubkeyAcceptedKeyTypes=ssh-rsa"
                fi
                echo "    ProxyCommand ssh -qAW %h:%p etk.ansible"
                echo ""
                ;;
            esac
        done

        IFS="${oldifs}"
    else
        echo "Directory '${GIT_CONNECT}' does not exist!"
    fi
    ;;
*)
    echo "Usage: ${0} { ericsson | internet | home | gen }"
    ;;
esac

exit 0

## END
