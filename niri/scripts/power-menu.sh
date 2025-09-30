#!/bin/bash

SELECTION="$(printf " Lock\n鈴 Suspend\n Log out\n Reboot\n Shutdown" \
    | wofi --dmenu --prompt "Power Menu:" --lines 5)"

case "$SELECTION" in
    *Lock) hyprlock ;;
    *Suspend) systemctl suspend ;;
    *Log*)  niri msg action quit ;;  # <-- Niri sí entiende esto
    *Reboot) systemctl reboot ;;
    *Shutdown) systemctl -i poweroff ;;
esac

