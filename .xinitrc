export LANG=zh_CN.UTF-8
export TERM=xterm-256color
# For xdg-open. Other alternatives are kde, xfce.
export DE=gnome

# Somehow ibus cannot be activated with C-Spc, although it can be done through the tray.
# export GTK_IM_MODULE=ibus
# export XMODIFIERS=@im=ibus
# export QT_IM_MODULE=ibus
# ibus-daemon -d -x
export XMODIFIERS=@im=SCIM
export GTK_IM_MODULE=scim
export QT_IM_MODULE=scim
scim -d

xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
xmodmap ~/.Xmodmap
urxvtd -q -f -o
LANG=en_US.utf8 urxvtc &
xhost +local:root
# Clipboard manager
parcellite &
trayer --edge top --align right --SetDockType true --expand true --width 7 &
gvtray &
emacsclient -c &
#firefox &
fqterm &
exec xmonad
