#!/bin/bash


# If not plugged and battery has 15 minuts left, display messaga
# Add cronjob
# crontab -e
# */15 * * * * /bin/bash /path/to/this/lowbat.sh
BATTINFO=`acpi -b`
if [[ `echo $BATTINFO | grep Discharging` && `echo $BATTINFO | cut -f 5 -d " "` < 00:15:00 ]] ; 
then
     DISPLAY=:0.0 /usr/bin/notify-send "low battery" "$BATTINFO"
fi
