#!/bin/sh

xset +fp /usr/local/share/fonts
setxkbmap -option ctrl:nocaps
xset r rate 200 40

xpywm 2>/tmp/xpywm.log &
xpymon 2>/tmp/xwpymon.log &
# xpylog 2>/tmp/xpylog.log &

export DefaultImModule=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx-autostart

urxvt -e tmux
