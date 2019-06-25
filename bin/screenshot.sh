#!/bin/bash

## path
export PATH=/bin:/usr/bin:/usr/local/bin:/opt/bin
export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin
export PATH=$PATH:/usr/X11R6/bin
export PATH=$PATH:/usr/games:/usr/local/games
export PATH=$PATH:$HOME

## date
DATE=$(date +%Y%m%d.%H%M%S)

## window id
WindowID=$(xwininfo | grep "Window id:" | awk '{print $4}')
echo "WindowID='${WindowID}'"
echo "WindowID='${WindowID}'" >> ~/Downloads/s.txt

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
#### (3b) scrot
scrot --quality 100 --select ~/screenshot_${DATE}.jpeg

##### (4) gnome
# gnome-screenshot -a

## message
echo "Screenshot taken to '~/screenshot_${DATE}.jpeg'"

## END
