#!/bin/bash

# --- 1. DEFINE APPEARANCE VARIABLES ---
# Font and size
DMENU_FONT="JetBrainsMono Nerd Font Mono:style=Bold:size=14"

# TokyoNight Colors (Hex codes from your dwm config.h)
COL_BG="#1a1b26" # background (Normal BG)
COL_FG="#a9b1d6" # foreground (Normal FG)
COL_BLK="#32344a" # black (Selected BG - a dark contrast)
COL_RED="#f7768e" # red (Selected FG - highlight color)

# --- 2. DEFINE MENU OPTIONS ---
OPTIONS="Shutdown\nReboot\nSuspend\nLogout\nCancel"

# --- 3. RUN DMENU WITH CUSTOM ARGUMENTS ---
CHOSEN=$(echo -e "$OPTIONS" | dmenu \
    -i \
    -p "Power:" \
    -fn "$DMENU_FONT" \
    -nb "$COL_BG" \
    -nf "$COL_FG" \
    -sb "$COL_BLK" \
    -sf "$COL_RED" \
)

# --- 4. EXECUTE CHOSEN COMMAND ---
case "$CHOSEN" in
    "Shutdown")
        systemctl poweroff ;;
    "Reboot")
        systemctl reboot ;;
    "Suspend")
        systemctl suspend ;;
    "Logout")
        # Ensure this is the correct command for your session (e.g., pkill dwm)
        pkill dwm ;;
    "Cancel")
        exit 0 ;;
    *)
        exit 0 ;;
esac
