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

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

emacs --daemon &
hsetroot -solid "#334050"
#nitrogen --restore
picom -m 1 &
xrandr_script.sh
xsetroot -name " $(whoami) @ $(hostname) "
xsetroot -cursor_name left_ptr
xfce4-power-manager &

#exec qtile
exec xmonad
#exec dwm
#exec awesome

#export DESKTOP_SESSION=plasma 
#exec startplasma-x11

#export XDG_SESSION_TYPE=x11
#export GDK_BACKEND=x11
#exec gnome-session
