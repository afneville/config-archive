#!/bin/bash

rofi_command="rofi -theme $HOME/.config/rofi/themes/theme_switcher.rasi"

# Themes
theme_1="Nord"
theme_2="Gruvbox"
theme_3="Dracula"
theme_4="Forest"
theme_5="Cyberpunk"
theme_6="Groot"
theme_7="Blade"
theme_8="Rick"

# Variable passed to rofi
options="$theme_1\n$theme_2\n$theme_3\n$theme_4\n$theme_5\n$theme_6\n$theme_7\n$theme_8"

chosen="$(echo -e "$options" | $rofi_command -p "Available Themes" -dmenu -selected-row 0)"
case $chosen in
    $theme_1)
        set_xresources.sh nord
        ;;
    $theme_2)
        set_xresources.sh gruvbox
        ;;
    $theme_3)
        set_xresources.sh dracula
        ;;
    $theme_4)
        set_xresources.sh forest
        ;;
    $theme_5)
        set_xresources.sh cyberpunk
        ;;
    $theme_6)
        set_xresources.sh groot
        ;;
    $theme_7)
        set_xresources.sh blade
        ;;
    $theme_8)
        set_xresources.sh rick
        ;;
esac
