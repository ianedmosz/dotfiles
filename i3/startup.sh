#!/usr/bin/env bash
# Espera a que i3 termine de iniciar
sleep 2

# ===== RESTAURAR TEMA Y WALLPAPER GUARDADOS =====
I3_DIR="$HOME/.config/i3"
CURRENT_THEME_FILE="$I3_DIR/.current_theme"
CURRENT_WALLPAPER_FILE="$I3_DIR/.current_wallpaper"
WALLPAPERS_DIR="$HOME/.config/wallpapers"

# Restaurar tema guardado
if [[ -f "$CURRENT_THEME_FILE" ]]; then
    saved_theme=$(cat "$CURRENT_THEME_FILE")
    echo "Restoring i3 theme: $saved_theme"
    ~/.config/i3/scripts/theme-wallpaper-switch.sh restore_theme "$saved_theme"
fi

# Restaurar wallpaper guardado
if [[ -f "$CURRENT_WALLPAPER_FILE" ]]; then
    saved_wallpaper=$(cat "$CURRENT_WALLPAPER_FILE")
    wallpaper_path="$WALLPAPERS_DIR/$saved_wallpaper"
    
    if [[ -f "$wallpaper_path" ]]; then
        echo "Restoring wallpaper: $saved_wallpaper"
        feh --bg-scale "$wallpaper_path" &
    else
        echo "Using default wallpaper"
        feh --bg-scale /home/ianedmosz/Downloads/final.jpg &
    fi
else
    feh --bg-scale /home/ianedmosz/Downloads/final.jpg &
fi

# ===== RESTO DE CONFIGURACIONES =====
# Picom
pgrep -x picom >/dev/null || picom -b &
# Polybar
~/.config/polybar/launch.sh &
# Touchpad
xinput set-prop "VEN_06CB:00 06CB:CE26 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "VEN_06CB:00 06CB:CE26 Touchpad" "libinput Tapping Button Mapping" 1 3

# Desactivar screen saver que puede interferir
xset s off

# Configurar DPMS: Standby, Suspend, Off (en segundos)
xset dpms 1200 1200 1200

# Opcional: activar DPMS si estaba desactivado
xset +dpms
