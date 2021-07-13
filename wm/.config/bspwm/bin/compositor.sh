#!/bin/sh

if [[ ! -z "$(pgrep -u $UID -x picom)" ]]; then

    killall picom

elif [[ -z "$(pgrep -u $UID -x picom)" ]]; then

    picom --experimental-backends -b

fi
