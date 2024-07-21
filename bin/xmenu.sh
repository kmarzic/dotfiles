#!/bin/bash

export PATH="${HOME}/bin:${HOME}/.local/bin:/bin:/usr/bin:/usr/local/bin:/sbin:/usr/sbin"
export _JAVA_AWT_WM_NONREPARENTING=1

${HOME}/.local/bin/xmenu -w <<EOF | sh &
 Terminal (st)         st
 Terminal (xterm)      xterm
 Terminal (urxvt)      urxvt
 Terminal (alacritty)  alacritty

 Firefox               firefox
 Firefox (private)     firefox --private-window
 Microsoft Edge        microsoft-edge
 Chromium              chromium
EOF

#### END
