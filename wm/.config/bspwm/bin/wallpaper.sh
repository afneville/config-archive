#!/bin/sh

if [[ $1 == "colour" ]]; then

    hsetroot -solid $(xrdb -query | grep 'background:'| awk '{print $NF}')

elif [[ $1 == "image" ]]; then

    nitrogen --restore
fi
