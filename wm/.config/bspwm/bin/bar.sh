#!/bin/sh

if [[ $1 == "kill" ]]; then

    killall polybar
    bspc config top_padding 0

elif [[ $1 == "start" ]]; then

    killall polybar
    while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
    polybar alex &
    bspc config top_padding 24

elif [[ $1 == "toggle" ]]; then

    if [[ -z "$(pgrep -u $UID -x polybar)" ]]; then

        polybar alex &
        bspc config top_padding 32

    else

        killall polybar
        bspc config top_padding 0
    fi

fi

