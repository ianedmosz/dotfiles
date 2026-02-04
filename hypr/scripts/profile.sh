#!/bin/bash

OPTIONS="Performance\nBalanced\nPower Saver\nShow Current\nExit"

CHOICE=$(echo -e "$OPTIONS" | rofi -dmenu -i -p "Power Profile")

case "$CHOICE" in
"Performance")
    powerprofilesctl set performance
    notify-send "Power Profile" "Performance mode enabled"
    ;;
"Balanced")
    powerprofilesctl set balanced
    notify-send "Power Profile" "Balanced mode enabled"
    ;;
"Power Saver")
    powerprofilesctl set power-saver
    notify-send "Power Profile" "Power Saver mode enabled"
    ;;
"Show Current")
    CURRENT=$(powerprofilesctl get)
    notify-send "Power Profile" "Current profile: $CURRENT"
    ;;
*)
    exit 0
    ;;
esac
