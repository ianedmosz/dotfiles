#!/usr/bin/env bash
set -euo pipefail
sleep 2

I3_DIR="$HOME/.config/i3"
CURRENT_THEME_FILE="$I3_DIR/.current_theme"
# Si no usas aún .current_wallpaper, comenta estas dos líneas:
CURRENT_WALLPAPER_FILE="$I3_DIR/.current_wallpaper"
WALLPAPERS_DIR="$HOME/.config/wallpapers"

# Restaurar tema (corrige la ruta real)
if [[ -f "$CURRENT_THEME_FILE" ]]; then
  saved_theme=$(cat "$CURRENT_THEME_FILE")
  echo "Restoring i3 theme: $saved_theme"
  "$I3_DIR/theme-wallpaper-switch.sh" restore_theme "$saved_theme" || true
fi

# === Fondo de pantalla ===
DEFAULT_WP="$HOME/.config/wallpapers/laino.png"  # <-- Asegúrate que este archivo existe

if [[ -f "$CURRENT_WALLPAPER_FILE" ]]; then
  saved_wallpaper=$(cat "$CURRENT_WALLPAPER_FILE")
  wallpaper_path="$WALLPAPERS_DIR/$saved_wallpaper"
  if [[ -f "$wallpaper_path" ]]; then
    feh --bg-scale "$wallpaper_path" > /tmp/feh.log 2>&1 &
  else
    echo "No existe $wallpaper_path; usando default" | tee -a /tmp/feh.log
    feh --bg-scale "$DEFAULT_WP" > /tmp/feh.log 2>&1 &
  fi
else
  # No hay .current_wallpaper; usa default conocido
  feh --bg-scale "$DEFAULT_WP" > /tmp/feh.log 2>&1 &
fi

# Picom
pgrep -x picom >/dev/null || picom -b &
# Polybar
~/.config/polybar/launch.sh &
# Touchpad
xinput set-prop "VEN_06CB:00 06CB:CE26 Touchpad" "libinput Tapping Enabled" 1 || true
xinput set-prop "VEN_06CB:00 06CB:CE26 Touchpad" "libinput Tapping Button Mapping" 1 3 || true

xset s off
xset dpms 1200 1200 1200
xset +dpms
