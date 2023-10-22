#!/bin/bash
export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin
if [ ! -z ${1} ] || [ ! -z ${2} ];
then
    if [ -e ${1} ] && [ ! -e ${2} ]
    then
        ## (1)
        # openssl aes-256-cbc -base64 -salt -in ${1} -out ${2}
        openssl aes-256-cbc -base64 -salt -pbkdf2 -in ${1} -out ${2}
        ## (2)
        # gpg -c ${1}
    else
        echo "File ${1} does not exist or file ${2} exists!"
    fi
else
    echo "Usage:"
    echo " ${0} unencrypted_file encrypted_file"
fi
## end
