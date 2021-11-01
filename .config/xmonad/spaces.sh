#!/bin/bash

export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin
DEFAULT_SPACES=5

###############################################################################
#### Functions
###############################################################################

function __echo_spaces()
{
    for (( i=1; i<=${DEFAULT_SPACES}; i++))
    do
        echo -n " "
    done
}

function __create_xpm_icon ()
{
    timestamp=$(date)
    pixels=$(for i in `seq $1`; do echo -n "."; done)

    cat << EOF > "$2"
/* XPM *
static char * trayer_pad_xpm[] = {
/* This XPM icon is used for padding in xmobar to */
/* leave room for trayer-srg. It is dynamically   */
/* updated by by trayer-padding-icon.sh which is run  */
/* by xmobar.                                     */
/* Created: ${timestamp} */
/* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
"$1 1 1 1",
/* Colors (none: transparent) */
". c none",
/* Pixels */
"$pixels"
};
EOF
}


###############################################################################
#### Main Program
###############################################################################

#### Width of the trayer window
width=$(xprop -name panel | grep 'program specified minimum size' | cut -d ' ' -f 5)

#### Icon file name
iconfile="/tmp/trayer-padding-${width}px.xpm"

#### If the desired icon does not exist create it
if [ ! -f $iconfile ]; then
    __create_xpm_icon ${width} ${iconfile}
fi

#### Default spaces
# if [[ ! -z ${2-} ]] && DEFAULT_SPACES=${2-}
# __echo_spaces

exit 0

###############################################################################
#### END
###############################################################################
