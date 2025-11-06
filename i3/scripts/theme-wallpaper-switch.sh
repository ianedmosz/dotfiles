#!/usr/bin/env bash
# ~/.config/i3/scripts/theme-wallpaper-switch.sh
# Selector simple de wallpapers con Rofi (sin theming hardcoded).
# - Guarda el último wallpaper en ~/.config/i3/.current_wallpaper
# - Usa tu configuración por defecto de Rofi
# - Intenta usar feh (luego nitrogen/xwallpaper/hsetroot/swaybg)

set -euo pipefail

I3_DIR="$HOME/.config/i3"
WALLPAPERS_DIR="$HOME/.config/wallpapers"
CURRENT_WALLPAPER_FILE="$I3_DIR/.current_wallpaper"

mkdir -p "$I3_DIR" "$WALLPAPERS_DIR"

# --- Utils ---
log() { printf '%s\n' "$*" >&2; }

# Lista wallpapers (rutas relativas a WALLPAPERS_DIR), incluye subdirectorios
get_available_wallpapers() {
  shopt -s nullglob
  local -a files=(
    "$WALLPAPERS_DIR"/*.{jpg,jpeg,png,webp,bmp,tiff}
    "$WALLPAPERS_DIR"/*/*.{jpg,jpeg,png,webp,bmp,tiff}
  )
  shopt -u nullglob
  if (( ${#files[@]} == 0 )); then
    return 1
  fi
  # Imprime rutas relativas, ordenadas y únicas
  for f in "${files[@]}"; do
    printf '%s\n' "${f#"$WALLPAPERS_DIR/"}"
  done | sort -u
}

# Aplica el wallpaper con el mejor setter disponible
set_wallpaper() {
  local rel="$1"
  local full="$WALLPAPERS_DIR/$rel"

  if [[ ! -f "$full" ]]; then
    log "Wallpaper no encontrado: $full"
    return 1
  fi

  if command -v feh >/dev/null 2>&1; then
    feh --bg-fill "$full"
  elif command -v nitrogen >/dev/null 2>&1; then
    nitrogen --set-zoom-fill "$full"
  elif command -v xwallpaper >/dev/null 2>&1; then
    xwallpaper --zoom "$full"
  elif command -v hsetroot >/dev/null 2>&1; then
    hsetroot -fill "$full"
  elif command -v swaybg >/dev/null 2>&1 && [[ "${XDG_SESSION_TYPE:-}" == "wayland" ]]; then
    pkill swaybg 2>/dev/null || true
    swaybg -i "$full" -m fill &
  else
    log "No hay setter de wallpaper disponible (feh/nitrogen/xwallpaper/hsetroot/swaybg)."
    return 1
  fi

  # Guarda el seleccionado
  printf '%s\n' "$rel" > "$CURRENT_WALLPAPER_FILE"
  return 0
}

# Selector con Rofi usando tu config por defecto (sin -theme/-theme-str)
choose_wallpaper_rofi() {
  local list
  if ! list="$(get_available_wallpapers)"; then
    # Notificación sencilla, sin rutas
    command -v notify-send &>/dev/null && notify-send "Wallpapers" "No se encontraron imágenes" -u normal
    log "No se encontraron wallpapers"
    exit 1
  fi

  # Sin -mesg (no mostrar directorio ni wallpaper actual)
  local selected
  selected="$(printf '%s\n' "$list" | rofi -dmenu -i \
    -p '🖼  Select wallpaper' \
    -no-custom -format s)"

  [[ -z "${selected:-}" ]] && exit 0

  if set_wallpaper "$selected"; then
    command -v notify-send &>/dev/null && notify-send "Wallpaper aplicado" "" -t 1500
  else
    command -v notify-send &>/dev/null && notify-send "Error" "No se pudo aplicar" -u critical
    exit 1
  fi
}

# Reaplica el último wallpaper guardado (para usar en autostart)
apply_current_if_any() {
  if [[ -f "$CURRENT_WALLPAPER_FILE" ]]; then
    local rel
    rel="$(cat "$CURRENT_WALLPAPER_FILE")"
    if set_wallpaper "$rel"; then
      exit 0
    else
      log "Fallo al reaplicar $rel"
      exit 1
    fi
  else
    # Silencioso si no hay guardado
    exit 0
  fi
}

# --- CLI ---
case "${1:-selector}" in
  ""|"selector"|"rofi") choose_wallpaper_rofi ;;
  "apply-current")       apply_current_if_any ;;
  "list")                get_available_wallpapers || true ;;
  "help"|"-h"|"--help")
    cat <<EOF
Uso: $(basename "$0") [selector|apply-current|list]
  selector       Abre Rofi y aplica el seleccionado (usa tu tema/config de Rofi por defecto)
  apply-current  Reaplica el último wallpaper guardado (para autostart)
  list           Lista wallpapers detectados (rutas relativas)
EOF
    ;;
  *) echo "Opción no reconocida: $1"; exit 1 ;;
esac
