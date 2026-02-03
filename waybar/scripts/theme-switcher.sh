#!/bin/bash

WAYBAR_CONFIG_DIR="$HOME/.config/waybar"
WOFI_CONFIG_DIR="$HOME/.config/wofi"

WAYBAR_THEMES_DIR="$WAYBAR_CONFIG_DIR/themes"
WOFI_THEMES_DIR="$WOFI_CONFIG_DIR/themes"

WAYBAR_STYLE="$WAYBAR_CONFIG_DIR/style.css"
WOFI_STYLE="$WOFI_CONFIG_DIR/style.css"

THEMES=$(ls "$WAYBAR_THEMES_DIR"/*.css 2>/dev/null | xargs -n1 basename | sed 's/.css$//')

if [ -z "$THEMES" ]; then
    notify-send "Theme Switcher" "No themes found"
    exit 1
fi

SELECTED=$(echo "$THEMES" | rofi -dmenu -p "Select Theme:")

# Cancelado
[ -z "$SELECTED" ] && exit 0

# 🔒 VALIDACIÓN IMPORTANTE
if [ ! -f "$WAYBAR_THEMES_DIR/$SELECTED.css" ]; then
    notify-send "Theme Switcher" "Theme not found: $SELECTED"
    exit 1
fi

# Waybar
if [ -f "$WAYBAR_STYLE" ]; then
    sed -i "1s|@import url(.*);|@import url(\"$WAYBAR_THEMES_DIR/$SELECTED.css\");|" "$WAYBAR_STYLE"
    killall waybar 2>/dev/null
    sleep 0.5
    waybar &
fi

# Wofi (opcional: valida también aquí si quieres)
if [ -f "$WOFI_STYLE" ] && [ -f "$WOFI_THEMES_DIR/$SELECTED.css" ]; then
    sed -i "1s|@import url(.*);|@import url(\"$WOFI_THEMES_DIR/$SELECTED.css\");|" "$WOFI_STYLE"
    pkill wofi 2>/dev/null
fi

notify-send "Theme Switcher" "Applied theme: $SELECTED"
