#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/TTF
xset +fp /usr/share/fonts/awesome-terminal-fonts 
xset +fp /home/perrierjouet/.local/share/fonts/ 
xset +fp /usr/share/fonts/ttf-iosevka 
xset fp rehash

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# french accent compose key
#setxkbmap -option compose:ralt

numlockx &

nitrogen --restore

mpd &
#emacs --daemon=instance1

#urxvtd -q -o -f


wmname LG3D
#. /etc/profile.d/jre.sh

export LANG=en_US.UTF-8
#sxhkd &
xsetroot -cursor_name left_ptr &
export PANEL_FIFO=/tmp/panel-fifo

#wal -R &

#rm -f /tmp/.dwm.fifo
#mkfifo /tmp/.dwm.fifo

#~/.config/scripts/bar/dwmbar.sh &
#exec dwm


#exec startxfce4

~/bin/bar/2bwmbar &
exec 2bwm

