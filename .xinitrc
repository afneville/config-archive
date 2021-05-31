#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then

    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then

    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

emacs --daemon &
#picom -m 1 &
#picom --experimental-backends &
xrandr_script.sh
# hsetroot -solid "#334050"
#hsetroot -solid "#404552"
#nitrogen --restore
#xsetroot -name " $(whoami) @ $(hostname) "
#xsetroot -cursor_name left_ptr
#xfce4-power-manager &

#exec qtile
#exec i3
#exec bspwm
#exec xmonad
#exec dwm
exec awesome

#export DESKTOP_SESSION=plasma 
#exec startplasma-x11

#export XDG_SESSION_TYPE=x11
#export GDK_BACKEND=x11
#exec gnome-session
