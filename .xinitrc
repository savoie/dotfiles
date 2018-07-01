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

unclutter --timeout 3 -b
xsetroot -cursor_name left_ptr
setxkbmap -option compose:ralt

sxlock -f "-misc-fantasque sans mono-medium-r-normal--0-100-0-0-M-0-*-*"

set SXHKD_SHELL="/bin/sh"
sxhkd &

compton --config ${HOME}/.config/compton/compton.conf -b
.local/bin/notifications &
/usr/bin/dropbox &

\emacs --daemon &

exec bspwm
