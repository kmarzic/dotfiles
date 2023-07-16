#!/bin/bash

export PATH="${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"

echo "------------------------------------------------------------------------------"
echo "SSH LOG script"
echo ""

command="${@}"
echo "command='${command}'"

date="$(date +%Y%m%d.%H%M%S)"
echo "date='${date}'"

host=$(echo ${command} | awk 'NF{ print $NF }')
echo "host='${host}'"

log_dir="${HOME}/.ssh/log"
echo "log_dir='${log_dir}'"

log_file="${log_dir}/ssh.${date}.${host}.log"
echo "log_file='${log_file}'"

echo "TERM=${TERM}"
echo "LC_TIME=${LC_TIME}"
echo "COLORTERM=${COLORTERM}"
echo "------------------------------------------------------------------------------"

if [ ! -d ${log_dir} ]
then
    mkdir ${log_dir}
else
    ${command} | tee ${log_file}
fi

## END
