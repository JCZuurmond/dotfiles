#!/bin/bash

#FILE=/tmp/screen_locked.png
#BLURTYPE="0x6"

lock() {
    if [ "$(date +%a | egrep 'wo|fr')" ] && [ $(date +%H) -gt 15 ] ;
    then
    	icon="$HOME/.beer.png"
    elif [ iw dev | grep Heineken --quiet ]
    then
    	icon="$HOME/.lock_logo_advanced_analytics.png"
    elif [ iw dev | grep Xebia --quiet ]
    then
    	icon="$HOME/.lock_logo_xccelerated.png"
    elif [ $RANDOM -lt 16383 ] 
    then
    	icon="$HOME/.lock_logo_xccelerated.png"
    else
    	icon="$HOME/.lock_logo_advanced_analytics.png"
    fi
    tmpbg='/tmp/screen.png'
    
    (( $# )) && { icon=$1; }
    
    scrot "$tmpbg"
    convert "$tmpbg" -scale 3.2% -scale 3130% "$tmpbg"
    convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
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
.
exit 0
