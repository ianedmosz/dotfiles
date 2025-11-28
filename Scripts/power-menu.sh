#!/bin/bash

# Define the power options, separated by newlines (\n)
OPTIONS="Shutdown\nReboot\nSuspend\nLogout\nCancel"

# Pipe the options into dmenu and capture the chosen command
CHOSEN=$(echo -e "$OPTIONS" | dmenu -i -p "Power:")

# Case statement to execute the command based on the choice
case "$CHOSEN" in
    "Shutdown")
        systemctl poweroff ;;
    "Reboot")
        systemctl reboot ;;
    "Suspend")
        systemctl suspend ;;
    "Logout")
        # This depends on your environment (e.g., kill your window manager)
        # Example for i3/dwm/Openbox:
        # i3-msg exit
        # openbox --exit
        # pkill dwm
        # You must choose the command that ends your session!
        pkill dwm ;;
    "Cancel")
        exit 0 ;;
    *)
        exit 0 ;;
esac
