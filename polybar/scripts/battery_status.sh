#!/bin/bash

BAT="/sys/class/power_supply/BAT0"
AC="/sys/class/power_supply/AC"

status=$(cat "$BAT/status")
ac=$(cat "$AC/online")
percentage=$(cat "$BAT/capacity")

get_icon() {
    local p=$1
    if [ $p -ge 80 ]; then echo ""; fi
    if [ $p -ge 60 ] && [ $p -lt 80 ]; then echo ""; fi
    if [ $p -ge 40 ] && [ $p -lt 60 ]; then echo ""; fi
    if [ $p -ge 20 ] && [ $p -lt 40 ]; then echo ""; fi
    if [ $p -lt 20 ]; then echo ""; fi
}

if [ "$status" = "Charging" ] || ([ "$status" = "Not charging" ] && [ "$ac" = "1" ]); then
    # Solo enchufe cuando está enchufado
    echo " $percentage%"
else
    # Solo icono de batería cuando está descargando
    icon=$(get_icon $percentage)
    echo "$icon $percentage%"
fi

