#!/bin/bash

#FILE=/tmp/screen_locked.png
#BLURTYPE="0x6"

lock() {
    #con="$HOME/.xlock/icon.png"
    tmpbg='/tmp/screen.png'
    
    (( $# )) && { icon=$1; }
    
    scrot "$tmpbg"
    convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
    #convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
    i3lock -i "$tmpbg"
}

case "$1" in
    lock)
        lock
        ;;
    logout)
        i3-msg exit
        ;;
    hibernate)
        lock && sudo pm-hibernate
        ;;
    suspend)
	# NOTE: pm-suspend is added to for sudo rights sudo visudo
        lock && sudo pm-suspend
        ;;
    reboot)
        reboot
        ;;
    poweroff)
        poweroff
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|reboot|shutdown}"
        exit 2
esac

exit 0
