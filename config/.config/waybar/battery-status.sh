#!/bin/bash
BATTERY=$(ls /sys/class/power_supply/ | grep -m 1 BAT)

if [[ -n "$BATTERY" ]]; then
    CAPACITY=$(cat /sys/class/power_supply/$BATTERY/capacity)
    STATUS=$(cat /sys/class/power_supply/$BATTERY/status)

    if [[ $STATUS == "Charging" ]]; then
	echo "$BATTERY: $CAPACITY% (charging)"
    elif [[ $STATUS == "Full" ]]; then
        echo "$BATTERY: Full"
    else
        echo "$BATTERY: $CAPACITY%"
    fi
else
    echo "No battery detected"
fi

