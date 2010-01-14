export LANG=zh_CN.UTF-8
export TERM=xterm-256color
# For xdg-open. Other alternatives are kde, xfce.
export DE=gnome

# The best pinyin for scim is scim-googlepinyin
# To use ibus, run ibus-setup and make sure the hotkey is set to Control+space, instead of Ctrl+space!!
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -d -x
# export XMODIFIERS=@im=SCIM
# export GTK_IM_MODULE=scim
# export QT_IM_MODULE=scim
# scim -d

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
# http://haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#XMonad_stops_responding_to_keys_.28usually_due_to_unclutter.29
unclutter -root &
exec xmonad
