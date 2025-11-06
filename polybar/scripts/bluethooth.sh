#!/usr/bin/env bash

# Bluetooth module for Polybar
# Shows icon depending on status and allows toggling

icon_on=""
icon_off=""

status=$(bluetoothctl show | grep "Powered" | awk '{print $2}')
connected=$(bluetoothctl info | grep "Device" | wc -l)

if [ "$status" = "yes" ]; then
    if [ "$connected" -gt 0 ]; then
        echo "%{F#00BFFF}$icon_on%{F-}"   # blue icon when connected
    else
        echo "%{F#66CCFF}$icon_on%{F-}"   # light blue when on but idle
    fi
else
    echo "%{F#555}$icon_off%{F-}"         # gray when off
fi
