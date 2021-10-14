#!/bin/bash

###############################################################################
## Variables
###############################################################################

## PATH
export PATH=$HOME/bin:/opt/ghc/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin

## date
DATE=$(date +%Y%m%d.%H%M%S)

## Exit
EXIT_OK=0
EXIT_ERROR=1


###############################################################################
## Functions
###############################################################################

function __window_id()
{
    ## window id
    WindowID=$(xwininfo | grep "Window id:" | awk '{print $4}')
    echo "WindowID='${WindowID}'"
    # echo "WindowID='${WindowID}'" >> ~/Downloads/s.txt
}

function __full()
{
    #### (1) xwd + ImageMagick
    # xwd -silent -id ${WindowID} | convert xwd:- ~/screenshot_${DATE}.png
    # xwd -silent -id ${WindowID} | convert xwd:- ~/screenshot_${DATE}.jpeg

    #### (2) ImageMagick
    # import -frame -window ${WindowID} ~/screenshot_${DATE}.png
    #### quality = 85
    # import -frame -window ${WindowID} ~/screenshot_${DATE}.jpeg
    #### quality = 100
    # import -frame -window ${WindowID} -quality 100 ~/screenshot_${DATE}.jpeg

    #### (3a) scrot
    # scrot --quality 100 --focused ~/screenshot_${DATE}.jpeg
    scrot --quality 100 --focused ~/screenshot_${DATE}.png

    ## message
    echo "Screenshot taken to '~/screenshot_${DATE}.png'"
}

function __selected()
{
    #### (3b) scrot
    # scrot --quality 100 --select ~/screenshot_${DATE}.jpeg
    # scrot --quality 100 --select ~/screenshot_${DATE}.png

    #### (4) gnome
    # gnome-screenshot -a

    #### (5) maim
    maim --select ~/screenshot_${DATE}.png

    ## message
    echo "Screenshot taken to '~/screenshot_${DATE}.png'"
}


###############################################################################
## Main
###############################################################################

#### Command Line
while getopts "fs" opt;
do
    case ${opt} in
        f)
            __full
            ;;
        s)
            __selected
            ;;
        :)
            __selected
            exit ${EXIT_ERROR}
            ;;
    esac
done

#### This tells getopts to move on to the next argument.
shift $((OPTIND-1))

#### Exit
exit ${EXIT_OK}

###############################################################################
## END
###############################################################################
