#!/bin/sh

DIR="$HOME/.config/wallpapers"

# Listar wallpapers (jpg/png/jpeg), mostrar solo nombre, pero usar la ruta completa
choice=$(find "$DIR" -maxdepth 1 -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) \
    | sort \
    | sed 's!.*/!!' \
    | dmenu -i -l 0 -p "Wallpaper:")

# Si cancelas dmenu, salir
[ -z "$choice" ] && exit 0

# Volver a armar la ruta completa
wall="$DIR/$choice"

# Aplicar con nitrogen
nitrogen --set-zoom-fill "$wall" --save

