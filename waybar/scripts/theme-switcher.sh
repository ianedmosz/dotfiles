#!/bin/bash

WAYBAR_CONFIG_DIR="$HOME/.config/waybar"
WOFI_CONFIG_DIR="$HOME/.config/wofi"

WAYBAR_THEMES_DIR="$WAYBAR_CONFIG_DIR/themes"
WOFI_THEMES_DIR="$WOFI_CONFIG_DIR/themes"

WAYBAR_STYLE="$WAYBAR_CONFIG_DIR/style.css"
WOFI_STYLE="$WOFI_CONFIG_DIR/style.css"

# Listar temas disponibles desde Waybar (o puedes cambiar a Wofi_THEMES_DIR si quieres)
THEMES=$(ls "$WAYBAR_THEMES_DIR"/*.css 2>/dev/null | xargs -n1 basename | sed 's/.css$//')

if [ -z "$THEMES" ]; then
    notify-send "Theme Switcher" "No themes found in $WAYBAR_THEMES_DIR"
    exit 1
fi

# Selector de tema usando Wofi
SELECTED=$(echo "$THEMES" | wofi --dmenu --prompt "Select Theme: ")

if [ -z "$SELECTED" ]; then
    exit 0
fi

# Actualizar Waybar
if [ -f "$WAYBAR_STYLE" ]; then
    sed -i "1s|@import url(.*);|@import url(\"$WAYBAR_THEMES_DIR/$SELECTED.css\");|" "$WAYBAR_STYLE"
    killall waybar 2>/dev/null
    sleep 0.5
    waybar &
fi

# Actualizar Wofi
if [ -f "$WOFI_STYLE" ]; then
    sed -i "1s|@import url(.*);|@import url(\"$WOFI_THEMES_DIR/$SELECTED.css\");|" "$WOFI_STYLE"

    # Matar Wofi actual para que recargue el tema al abrirlo
    pkill wofi 2>/dev/null
fi

notify-send "Theme Switcher" "Applied theme: $SELECTED"

