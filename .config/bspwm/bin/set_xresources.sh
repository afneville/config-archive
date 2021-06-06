#!/usr/bin/env bash

# apply new color-scheme to gui

set_style () {
	
	# apply color-scheme
	cat $HOME/.Xresources.d/themes_available/${1} > $HOME/.Xresources.d/colours
	
	# reload window manager config

}

set_style "${1}"
