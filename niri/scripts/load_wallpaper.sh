#!/bin/bash

# Archivo donde se guarda el último wallpaper y el modo
PERSISTENCE_FILE="$HOME/.config/fuzzel/last_wallpaper.txt"

# Verifica si existe el archivo de persistencia
if [[ -f "$PERSISTENCE_FILE" ]]; then
    # Primera línea = ruta de la imagen
    IMG=$(head -n1 "$PERSISTENCE_FILE")
    # Segunda línea = modo de swaybg (fill, fit, stretch, etc.)
    MODE=$(tail -n1 "$PERSISTENCE_FILE")

    # Aplica el wallpaper
    swaybg -m "$MODE" -i "$IMG" &
else
    notify-send "Wallpaper Loader" "No se encontró $PERSISTENCE_FILE"
fi

