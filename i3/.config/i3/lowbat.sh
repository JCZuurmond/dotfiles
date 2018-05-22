#!/bin/bash

# Needed for notify-send to properly work
environs=`pidof dbus-daemon | tr ' ' '\n' | awk '{printf "/proc/%s/environ ", $1}'`
export DBUS_SESSION_BUS_ADDRESS=`cat $environs 2>/dev/null | tr '\0' '\n' | grep DBUS_SESSION_BUS_ADDRESS | cut -d '=' -f2-`
export DISPLAY=:0.0

# If not plugged and battery has 15 minuts left, display messaga
# Add cronjob
# crontab -e
# */15 * * * * /bin/bash /path/to/this/lowbat.sh
BATTINFO=`acpi -b`
if [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:15:00 ]] ; 
then
     /usr/bin/notify-send -u critical -t 60000 "low battery" "$BATTINFO"
fi
