#!/bin/sh

# xinit is hardcoded to use sh, so this hack is the only way to run .xinitrc
# with bash. Relevant thread:
# https://www.reddit.com/r/linuxquestions/comments/d0ktbm/zsh_process_substitution_not_working_in_xinitrc/
if [ x"$BASH" != x"/bin/bash" ]; then
    exec /bin/bash ${HOME}/.xinitrc
fi

# Add absolute and relative timestamps to output and redirect to ~/.Xoutput.
exec > >(ts -s "(+%.s)]" | ts "[%Y-%m-%d %H:%M:%S" > ${HOME}/.Xoutput) 2>&1
set -x

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

set SXHKD_SHELL="/bin/sh"
sxhkd &

feh --bg-fill ${HOME}/.local/share/img/dreary-neon.jpg &
wal -f ${HOME}/.config/wal/colorschemes/base16-custom.json &

picom --config ${HOME}/.config/picom/picom.conf --xrender-sync-fence --use-ewmh-active-win -b
lemonbar-run &
.local/bin/notifications &

\emacs --daemon &
/usr/bin/dropbox &

exec bspwm
