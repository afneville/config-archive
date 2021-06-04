#!/bin/sh

if [[ $1 == "kill" ]]; then

    killall polybar
    bspc config top_padding 0

elif [[ $1 == "start" ]]; then

    pgrep -x polybar > /dev/null || polybar alex &
    bspc config top_padding 24

fi
