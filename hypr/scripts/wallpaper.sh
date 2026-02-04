#!/bin/bash

WALLPAPER_DIR="$HOME/.config/wallpapers"
MENU_PROMPT="Wallpaper"
CACHE_FILE="$HOME/.cache/current_wallpaper"

# Lanzar swww-daemon si no está corriendo
if ! pgrep -x swww-daemon >/dev/null; then
    swww-daemon &
    sleep 0.5
fi

# Selector con rofi
wallpaper=$(find "$WALLPAPER_DIR" -maxdepth 1 -type f \
    \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) |
    sort | rofi -dmenu -p "$MENU_PROMPT")

# Si no eliges nada, salir
[[ -z "$wallpaper" ]] && exit 0

# Guardar el wallpaper elegido
echo "$wallpaper" >"$CACHE_FILE"

# Aplicar wallpaper con transición
swww img "$wallpaper" \
    --transition-type grow \
    --transition-duration 0.7 \
    --transition-fps 60
