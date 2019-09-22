#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"

function __check_file()
{
    if [[ -e ${WTTR_FILE} ]]
    then
        file=$(find ${WTTR_FILE} -type f -cmin -${AGE})
        # echo "file='${file}'"

        if [[ "${file}" == "" ]]
        then
            # echo "file age not ok"
            __forecast
        else [[ "${file}" != "" ]]
            # echo "file age ok"
            echo -e "$(cat ${WTTR_FILE})"
        fi
    else
        __forecast
    fi
}

function __forecast()
{
    #### (1)
    # route=$(netstat -rn | grep "^0.0.0.0" | awk '{ print $2 }')
    #### (2)
    route=$(ip r | grep "^default" | awk '{ print $3 }')
    # echo "route='${route}'"

    ping -c1 -W1 ${route} &> /dev/null
    ping_status=${?}

    if [[ ${ping_status} -eq 0 ]]
    then
        # forecast="forecast"
        # forecast=$(curl -s http://wttr.in/Zagreb?format=1)
        # forecast=$(curl -s http://wttr.in/Zagreb?format=2)
        forecast=$(curl -s http://wttr.in/Zagreb?format='%l:+%c+%t+%h+%w+%m' > ${WTTR_FILE})
        echo -e "${forecast}"
    else
        echo ""
    fi
}

__check_file

#### END
