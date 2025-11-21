#!/usr/bin/env bash
# ~/.config/i3/scripts/theme-wallpaper-switch.sh
# Simple wallpaper selector with Rofi (no hardcoded theming).
# - Stores last wallpaper in ~/.config/i3/.current_wallpaper
# - Uses your default Rofi config
# - Tries feh (then nitrogen/xwallpaper/hsetroot/swaybg)

set -euo pipefail

I3_DIR="$HOME/.config/i3"
WALLPAPERS_DIR="$HOME/.config/wallpapers"
CURRENT_WALLPAPER_FILE="$I3_DIR/.current_wallpaper"

mkdir -p "$I3_DIR" "$WALLPAPERS_DIR"

# --- Utils ---
log() { printf '%s\n' "$*" >&2; }

# List wallpapers (paths relative to WALLPAPERS_DIR), including subdirs
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
  # Print relative, sorted, unique
  for f in "${files[@]}"; do
    printf '%s\n' "${f#"$WALLPAPERS_DIR/"}"
  done | sort -u
}

# Apply wallpaper with best available setter
set_wallpaper() {
  local rel="$1"
  local full="$WALLPAPERS_DIR/$rel"

  if [[ ! -f "$full" ]]; then
    log "Wallpaper not found: $full"
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
    log "No wallpaper setter available (feh/nitrogen/xwallpaper/hsetroot/swaybg)."
    return 1
  fi

  # Save selected
  printf '%s\n' "$rel" > "$CURRENT_WALLPAPER_FILE"
  return 0
}

# Rofi selector using your default config (with font override)
choose_wallpaper_rofi() {
  local list
  if ! list="$(get_available_wallpapers)"; then
    # Simple notification, no paths
    if command -v notify-send &>/dev/null; then
      notify-send "Wallpapers" "No images found" -u normal
    fi
    log "No wallpapers found"
    exit 1
  fi

  # No -mesg (don’t show dir or current wallpaper)
  local selected
  selected="$(printf '%s\n' "$list" | rofi -dmenu -i \
    -p '🖼  Select wallpaper' \
    -no-custom -format s \
    -lines 12 \
    -width 40 \
    -theme-str ' * { font: "JetBrainsMono Nerd Font 20"; }')"

  [[ -z "${selected:-}" ]] && exit 0

  if set_wallpaper "$selected"; then
    if command -v notify-send &>/dev/null; then
      # Show filename in the notification
      notify-send "Wallpaper changed" "$selected" -t 1500
    fi
  else
    if command -v notify-send &>/dev/null; then
      notify-send "Error" "Failed to apply wallpaper" -u critical
    fi
    exit 1
  fi
}

# Reapply last saved wallpaper (for autostart)
apply_current_if_any() {
  if [[ -f "$CURRENT_WALLPAPER_FILE" ]]; then
    local rel
    rel="$(cat "$CURRENT_WALLPAPER_FILE")"
    if set_wallpaper "$rel"; then
      exit 0
    else
      log "Failed to reapply $rel"
      exit 1
    fi
  else
    # Silent if none saved
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
Usage: $(basename "$0") [selector|apply-current|list]
  selector       Open Rofi and apply selected wallpaper (uses your default Rofi config)
  apply-current  Reapply the last saved wallpaper (for autostart)
  list           List detected wallpapers (relative paths)
EOF
    ;;
  *) echo "Unknown option: $1"; exit 1 ;;
esac
