#!/bin/bash

CACHE_FILE="$HOME/.cache/current_wallpaper"

# No hacer nada si no hay wallpaper guardado
[[ ! -f "$CACHE_FILE" ]] && exit 0

WALLPAPER=$(cat "$CACHE_FILE")

# Arrancar daemon si no está
if ! pgrep -x swww-daemon >/dev/null; then
    swww-daemon &
    sleep 0.5
fi

# Cargar wallpaper
swww img "$WALLPAPER" \
    --transition-type grow \
    --transition-duration 0.7 \
    --transition-fps 60
