#!/bin/bash

# fetch colors from xrdb and set them in other applications.

get_colours () {

	FOREGROUND="$(xrdb -query | grep 'foreground:'| awk '{print $NF}')"
	BACKGROUND="$(xrdb -query | grep 'background:'| awk '{print $NF}')"
	CURSOR="$(xrdb -query | grep 'cursorColor:'| awk '{print $NF}')"
	BLACK="$(xrdb -query | grep 'color0:'| awk '{print $NF}')"
	RED="$(xrdb -query | grep 'color1:'| awk '{print $NF}')"
	GREEN="$(xrdb -query | grep 'color2:'| awk '{print $NF}')"
	YELLOW="$(xrdb -query | grep 'color3:'| awk '{print $NF}')"
	BLUE="$(xrdb -query | grep 'color4:'| awk '{print $NF}')"
	MAGENTA="$(xrdb -query | grep 'color5:'| awk '{print $NF}')"
	CYAN="$(xrdb -query | grep 'color6:'| awk '{print $NF}')"
	WHITE="$(xrdb -query | grep 'color7:'| awk '{print $NF}')"
	ALTBLACK="$(xrdb -query | grep 'color8:'| awk '{print $NF}')"
	ALTRED="$(xrdb -query | grep 'color9:'| awk '{print $NF}')"
	ALTGREEN="$(xrdb -query | grep 'color10:'| awk '{print $NF}')"
	ALTYELLOW="$(xrdb -query | grep 'color11:'| awk '{print $NF}')"
	ALTBLUE="$(xrdb -query | grep 'color12:'| awk '{print $NF}')"
	ALTMAGENTA="$(xrdb -query | grep 'color13:'| awk '{print $NF}')"
	ALTCYAN="$(xrdb -query | grep 'color14:'| awk '{print $NF}')"
	ALTWHITE="$(xrdb -query | grep 'color15:'| awk '{print $NF}')"
}

bar_colours () {

    # set the colours used in polybar 

	cat > $HOME/.config/polybar/colours <<- EOF	
	[color]

	foreground = ${FOREGROUND}
	background = ${BACKGROUND}
	black = ${BLACK}
	red = ${RED}
	green = ${GREEN}
	yellow = ${YELLOW}
	blue = ${BLUE}
	magenta = ${MAGENTA}
	cyan = ${CYAN}
	white = ${WHITE}
	altblack = ${ALTBLACK}
	altred = ${ALTRED}
	altgreen = ${ALTGREEN}
	altyellow = ${ALTYELLOW}
	altblue = ${ALTBLUE}
	altmagenta = ${ALTMAGENTA}
	altcyan = ${ALTCYAN}
	altwhite = ${ALTWHITE}
	EOF
}

term_colours () {

    # set the alacritty terminal colours.

	cat > $HOME/.config/alacritty/colours.yml <<- _EOF_
		## Colors configuration
		colors:
		  # Default colors
		  primary:
		    background: '${BACKGROUND}'
		    foreground: '${FOREGROUND}'

		  # Normal colors
		  normal:
		    black:   '${BLACK}'
		    red:     '${RED}'
		    green:   '${GREEN}'
		    yellow:  '${YELLOW}'
		    blue:    '${BLUE}'
		    magenta: '${MAGENTA}'
		    cyan:    '${CYAN}'
		    white:   '${WHITE}'

		  # Bright colors
		  bright:
		    black:   '${ALTBLACK}'
		    red:     '${ALTRED}'
		    green:   '${ALTGREEN}'
		    yellow:  '${ALTYELLOW}'
		    blue:    '${ALTBLUE}'
		    magenta: '${ALTMAGENTA}'
		    cyan:    '${ALTCYAN}'
		    white:   '${ALTWHITE}'
	_EOF_
}

launcher_colours () {

    # set the colours in rofi.

	cat > $HOME/.config/rofi/themes/colours.rasi <<- EOF	
	* {
		background:			${BACKGROUND};
		foreground:			${FOREGROUND};
		selected:			${BLUE};
		urgent:				${RED};
		on:					${GREEN};
		off:				${RED};
        comment:            #8A8F99;
	}
	EOF
}

main () {

    # run all the functions.

	get_colours
	bar_colours
	term_colours
	launcher_colours
}

main
