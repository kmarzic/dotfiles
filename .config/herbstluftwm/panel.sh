#!/bin/bash

function quote()
{
    local q="$(printf '%q ' "$@")"
    printf '%s' "${q% }"
}

function hc()
{
    "${herbstclient_command[@]:-herbstclient}" "${@}" ;
}

function uniq_linebuffered()
{
    awk '$0 != l { print ; l=$0 ; fflush(); }' "${@}"
}

hc_quoted="$(quote "${herbstclient_command[@]:-herbstclient}")"

function __load()
{
    #### Load
    ncpu="$(cat /proc/cpuinfo | grep processor | wc -l)"
    load="$(cat /proc/loadavg | awk {' print $1 '})"
    load_percent="$(echo "scale=2; ${load}/${ncpu}*100" | bc -l | sed -e "s/\..*//g")"

    [[ $(echo "${load_percent} <= 50" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})CPU: ^fg(${GREEN})${load}^fg(${NORMAL})"
    [[ $(echo "${load_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${load_percent} < 80" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})CPU: ^fg(${YELLOW})${load}^fg(${NORMAL})"
    [[ $(echo "${load_percent} >= 80" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})CPU: ^fg(${RED})${load}^fg(${NORMAL})"
}

function __temp()
{
    temp="$(acpi -t | awk '{ print $4 " °C" }')"
    temp_dec="$(acpi -t | awk '{ print $4 }' | sed -e "s/\..*//g")"

    [[ -z ${temp} ]] && echo "Temp: ^fg(${RED})-^fg(${NORMAL})"

    [[ ! -z ${temp} ]] && [[ $(echo "${temp_dec} <= 40" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})Temp: ^fg(${GREEN})${temp}^fg(${NORMAL})"
    [[ ! -z ${temp} ]] && [[ $(echo "${temp_dec} >  40" | bc -l) -eq 1 ]] && [[ $(echo "${temp_dec} < 60" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL}Temp: ^fg(${YELLOW})${temp}^fg(${NORMAL})"
    [[ ! -z ${temp} ]] && [[ $(echo "${temp_dec} >= 60" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})Temp: ^fg(${RED})${temp}^fg(${NORMAL})"
}

function __memory()
{
    #### Memory
    mem_free=$(cat /proc/meminfo | grep "MemFree:" | awk {' print $2'})
    mem_total=$(cat /proc/meminfo | grep "MemTotal:" | awk {' print $2'})
    mem_available=$(cat /proc/meminfo | grep "MemAvailable:" | awk {' print $2'})
    mem_percent="$(echo "scale=2; ${mem_available}/${mem_total}*100" | bc -l | sed -e "s/\..*//g")"

    [[ $(echo "${mem_percent} <= 50" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})MEM: ^fg(${RED})${mem_percent}%^fg(${NORMAL})"
    [[ $(echo "${mem_percent} >  50" | bc -l) -eq 1 ]] && [[ $(echo "${mem_percent} < 90" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})MEM: ^fg(${YELLOW})${mem_percent}%^fg(${NORMAL})"
    [[ $(echo "${mem_percent} >= 90" | bc -l) -eq 1 ]] && echo "^fg(${NORMAL})MEM: ^fg(${GREEN})${mem_percent}%^fg(${NORMAL})"
}

function __battery()
{
    battery_enabled=$(find /sys/class/power_supply | grep -i bat | wc -l)

    if [[ ${battery_enabled} -eq 0 ]]
    then
        echo "BAT: ^fg(${RED})-^fg(${NORMAL})"
    else
        if [[ $(acpi --battery | grep "Discharging" | wc -l) -eq 1 ]]
        then
            battery_status="^fg(${CYAN})D^fg(${NORMAL})"
        elif [[ $(acpi --battery | grep "Charging" | wc -l) -eq 1 ]]
        then
            battery_status="^fg(${CYAN})C^fg(${NORMAL})"
        else
            battery_status="^fg(${CYAN})F^fg(${NORMAL})"
        fi
        battery="$(acpi --battery | cut -d, -f2 | sed -e "s/ //g;s/%//g")"

        [[ ${battery} -le 25 ]] && echo "^fg(${NORMAL})BAT: ${battery_status} ^fg(${RED})${battery}%^fg(${NORMAL})"
        [[ ${battery} -gt 25 ]] && [[ ${battery} -lt 80 ]] && echo "^fg(${NORMAL})BAT: ${battery_status} ^fg(${YELLOW})${battery}%^fg(${NORMAL})"
        [[ ${battery} -ge 80 ]] && echo "^fg(${NORMAL})BAT: ${battery_status} ^fg(${GREEN})${battery}%^fg(${NORMAL})"
    fi
}

function __weather()
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
            echo "$(cat ${WTTR_FILE})"
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
        echo "${forecast}"
    else
        echo ""
    fi
}

#### monitor
monitor=${1:-0}
GEOMETRY=( $(hc monitor_rect "${monitor}") )
if [ -z "${GEOMETRY}" ];
then
    echo "Invalid monitor ${monitor}"
    exit 1
fi

#### GEOMETRY has the format: WxH+X+Y
PANEL_WIDTH=${GEOMETRY[2]}
# PANEL_HEIGHT=16
PANEL_HEIGHT=16
x=${GEOMETRY[0]}
y=$((${GEOMETRY[3]} - ${GEOMETRY[1]} - ${PANEL_HEIGHT}))
echo "x: $x, y: $y" >> /tmp/hlog
# FONT="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"
FONT="-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso10646-1"
# FONT="fixed"
# FONT="terminus-12"
BGCOLOR=$(hc get frame_border_normal_color)
SELBG=$(hc get window_border_active_color)
SELFG='#101010'

ICONS="${HOME}/.config/herbsluftwm/icons"
AGE=15
WTTR_FILE="/var/tmp/wttr.txt"
RED='#ff0000'
GREEN='#00ff00'
YELLOW='#ffff00'
CYAN='#00ffff'
NORMAL='#efefef'

####
# Try to find textwidth binary.
# In e.g. Ubuntu, this is named dzen2-textwidth.
if [ -e "$(which textwidth 2> /dev/null)" ];
then
    textwidth="textwidth";
elif [ -e "$(which dzen2-textwidth 2> /dev/null)" ];
then
    textwidth="dzen2-textwidth";
else
    echo "This script requires the textwidth tool of the dzen2 project."
    exit 1
fi

####
## true if we are using the svn version of dzen2
## depending on version/distribution, this seems to have version strings like
## "dzen-" or "dzen-x.x.x-svn"
if dzen2 -v 2>&1 | head -n 1 | grep -q '^dzen-\([^,]*-svn\|\),';
then
    dzen2_svn="true"
else
    dzen2_svn=""
fi

if awk -Wv 2>/dev/null | head -1 | grep -q '^mawk';
then
    ## mawk needs "-W interactive" to line-buffer stdout correctly
    ## http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=593504
    uniq_linebuffered() {
      awk -W interactive '$0 != l { print ; l=$0 ; fflush(); }' "${@}"
    }
else
    ## other awk versions (e.g. gawk) issue a warning with "-W interactive", so
    ## we don't want to use it there.
    uniq_linebuffered() {
      awk '$0 != l { print ; l=$0 ; fflush(); }' "${@}"
    }
fi

#### Add a space for the panel at the bottom of the screen.
hc pad ${monitor} 0 0 ${PANEL_HEIGHT} 0

{
    #### Event generator
    ## based on different input data (mpc, date, hlwm hooks, ...) this generates events, formed like this:
    ##   <eventname>\t<data> [...]
    ## e.g.
    ##   date    ^fg(#efefef)18:33^fg(#909090), 2013-10-^fg(#efefef)29

    ## Time
    while true;
    do
        #### Time
        date +$'date ^fg(#efefef)%H:%M:%S^fg(#909090) %a %Y-%m-^fg(#efefef)%d'

        LOAD=$(__load)
        TEMP=$(__temp)

        #### sleep
        sleep 1 || break
    done > >(uniq_linebuffered) &

    ## Herbsluftwm events
    childpid1=$!
    hc --idle
    kill ${childpid1}

} 2>> /tmp/hlog | {
    TAGS=( $(hc tag_status ${monitor}) )
    date=""
    windowtitle=""
    stats=""

    while true;
    do
        bordercolor="#26221C"
        separator="^bg()^fg(${SELBG})|"

        #### draw tags
        for i in "${TAGS[@]}";
        do
            case ${i:0:1} in
                '#')
                    #### Tag is focused, and so is monitor
                    echo -n "^bg(${SELBG})^fg(${SELFG})"
                    ;;
                '+')
                    #### Tag is focused, but monitor is not
                    echo -n "^bg(#9CA668)^fg(#141414)"
                    ;;
                ':')
                    #### Tag is not focused, and is not empty
                    echo -n "^bg()^fg(#ffffff)"
                    ;;
                '!')
                    #### The tag contains an urgent window
                    echo -n "^bg(#FF0675)^fg(#141414)"
                    ;;
                *)
                    #### Otherwise
                    echo -n "^bg()^fg(#ababab)"
                    ;;
            esac

            if [ ! -z "${dzen2_svn}" ];
            then
                ## clickable tags if using SVN dzen
                echo -n "^ca(1,${hc_quoted} focus_monitor \"${monitor}\" && "
                echo -n "${hc_quoted} use \"${i:1}\") ${i:1} ^ca()"
            else
                ## non-clickable tags if using older dzen
                echo -n " ${i:1} "
            fi
        done
        echo -n "${separator}"
        echo -n "^bg()^fg() ${windowtitle//^/^^}"

        #### small adjustments
        # right="${separator} $(__load) ${separator} $(__temp) ${separator} $(__memory) ${separator} $(__battery) ${separator} $(__weather) ${separator}${date} ${separator}"
        right="${separator} ${LOAD} ${TEMP} ${date} ${separator}"
        right_text_only=$(echo -n "${right}" | sed 's.\^[^(]*([^)]*)..g')

        #### get width of right aligned text.. and add some space..
        width=$(${textwidth} "${FONT}" "${right_text_only}     ")
        # echo "width: ${width}" >> /tmp/hlog
        echo -n "^pa($((${PANEL_WIDTH} - ${width})))${right}"
        echo

        #### Data handling
        ## This part handles the events generated in the event loop, and sets
        ## internal variables based on them. The event and its arguments are
        ## read into the array cmd, then action is taken depending on the event
        ## name.
        ## "Special" events (quit_panel/togglehidepanel/reload) are also handled
        ## here.

        #### wait for next event
        read line || break
        cmd=( ${line} )

        #### find out event origin
        case "${cmd[0]}" in
            tag*)
                # echo "reseting tags" >&2
                TAGS=( $(hc tag_status ${monitor}) )
                ;;
            date)
                # echo "reseting date" >&2
                date="^fg()^i(${ICONS}/clock.xbm) "
                date="${date}${cmd[@]:1}"
                ;;
            quit_panel)
                exit
                ;;
            reload)
                exit
                ;;
            focus_changed|window_title_changed)
                windowtitle="${cmd[@]:2}"
                ;;
        esac
    done
} 2>> /tmp/hlog | dzen2 -w ${PANEL_WIDTH} -x ${x} -y ${y} -fn "${FONT}" -h ${PANEL_HEIGHT} \
    -e "button3=;button4=exec:${hc_quoted} use_index -1;button5=exec:${hc_quoted} use_index +1" \
    -ta l -bg "${BGCOLOR}" -fg '#efefef' -e 2>&1 >> /tmp/hlog

#### END
