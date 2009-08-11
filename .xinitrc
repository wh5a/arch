export LANG=zh_CN.UTF-8
export TERM=xterm-256color
export GTK_IM_MODULE=ibus
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE=ibus
ibus-daemon -d -x
xrdb -merge .Xresources
xsetroot -cursor_name left_ptr
urxvtd -q -f -o
urxvtc &
xhost +local:root
exec xmonad
