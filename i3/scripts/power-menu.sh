#!/usr/bin/env bash

SELECTION="$(
  printf " Lock\n鈴 Suspend\n Log out\n Reboot\n Shutdown" \
    | rofi -dmenu \
        -p "Power Menu" \
        -i \
        -lines 5 \
        -width 25 \
        -theme-str ' * { font: "JetBrainsMono Nerd Font 20"; }'
)"

case "$SELECTION" in
  *Lock)     
      # Si tienes i3lock, úsalo. Si no, cámbialo.
      if command -v i3lock >/dev/null 2>&1; then
          i3lock -c 000000
      else
          notify-send "No i3lock installed"
      fi
      ;;
  *Suspend)  
      systemctl suspend
      ;;
  *Log\ out | *Log\ Out | *Log*)  
      i3-msg exit
      ;;
  *Reboot)   
      systemctl reboot
      ;;
  *Shutdown) 
      systemctl -i poweroff
      ;;
esac

