#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap


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


# monitor=`xrandr | grep 'HDMI-A-0 connected'`
# if [ ! -z  "$monitor" ]; then
#     # make external monitor the primary
#     # xrandr --output HDMI-A-0 --primary --mode 1920x1080 --right-of eDP
#     # disable one monitor
#     # xrandr --output HDMI1-A-0 --mode 1920x1080 --output eDP --off
# fi

# emacs --daemon &
# xfce4-power-manager &
# picom -m 1 &
# picom --experimental-backends &
# numlockx &
# hsetroot -solid $(xrdb -query | grep 'background:'| awk '{print $NF}')
# nitrogen --restore
# xsetroot -name "  $(whoami) @ $(hostname) "
# xsetroot -cursor_name left_ptr
# killall sxhkd
# sxhkd -c ~/.config/sxhkd/application_bindings &
# exec qtile
# exec i3
# exec bspwm
# exec xmonad
# /home/alex/code/dwm/dwm_bar.sh &
# exec dwm
# exec awesome
export DESKTOP_SESSION=plasma
exec startplasma-x11
exec dbus-launch --exit-with-session startplasma-x11
# export XDG_SESSION_TYPE=x11
# export GDK_BACKEND=x11
# exec gnome-session
# exec cinnamon-session
# exec mate-session
# exec startxfce4
