#!/bin/sh

function efficient_mode() {

    echo "background_opacity: 1.0" > ~/.config/alacritty/opacity.yml
    touch ~/.config/alacritty/alacritty.yml
    killall picom

}

function fashion_mode() {

    echo "background_opacity: 0.7" > ~/.config/alacritty/opacity.yml
    touch ~/.config/alacritty/alacritty.yml
    picom --experimental-backends -b

}

if [[ $1 == "toggle" ]]; then 

    if [[ "$(pgrep -u $UID -x picom)" ]]; then

        efficient_mode

    elif [[ -z "$(pgrep -u $UID -x picom)" ]]; then

        fashion_mode

    fi

elif [[ $1 == "start" ]]; then 

    fashion_mode

elif [[ $1 == "kill" ]]; then 
    
    efficient_mode

fi
