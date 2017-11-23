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

# set wal (no need for whole wal-set command on initial startup)
wal -i ${HOME}/.local/share/img/ -o ${HOME}/.local/bin/wal-set

compton --config ${HOME}/.config/compton/compton.conf -b
unclutter --timeout 3 -b
xsetroot -cursor_name left_ptr
.local/bin/notifications &
/usr/bin/dropbox &

\emacs --daemon

exec bspwm
