#!/usr/bin/env bash
# ~/.config/polybar/scripts/theme-switch.sh

set -euo pipefail

POLYBAR_DIR="$HOME/.config/polybar"
THEMES_DIR="$POLYBAR_DIR/themes"
COLORS_FILE="$POLYBAR_DIR/colors.ini"
CURRENT_THEME_FILE="$POLYBAR_DIR/.current_theme"

notify() { command -v notify-send &>/dev/null && notify-send "$@"; }

if [[ ! -d "$THEMES_DIR" ]]; then
  notify "Error" "Themes directory not found: $THEMES_DIR" -u critical
  echo "Error: Themes directory not found: $THEMES_DIR" >&2
  exit 1
fi

get_available_themes() {
  local -a themes=()
  # Si no hay .ini, no pasa nada; devolvemos "default"
  while IFS= read -r f; do
    [[ -n "$f" ]] && themes+=("${f%.ini}")
  done < <(find "$THEMES_DIR" -maxdepth 1 -type f -name '*.ini' -printf '%f\n' | sort -u)

  if [[ ${#themes[@]} -eq 0 ]]; then
    printf '%s\n' "default"
  else
    printf '%s\n' "${themes[@]}"
  fi
}

get_current_theme() { [[ -f "$CURRENT_THEME_FILE" ]] && cat "$CURRENT_THEME_FILE" || echo "default"; }

# -------- FIX: no reventar si se llama sin $1 --------
get_theme_info() {
  local theme_name="${1-}"
  [[ -z "${theme_name}" ]] && { echo "unknown (missing)"; return 0; }

  local theme_file="$THEMES_DIR/${theme_name}.ini"
  [[ ! -f "$theme_file" ]] && { echo "$theme_name (missing)"; return 0; }

  local bg accent
  bg=$(grep -E '^\s*background\s*=' "$theme_file" | head -1 | cut -d= -f2- | xargs || true)
  accent=$(grep -E '^\s*accent\s*=' "$theme_file" | head -1 | cut -d= -f2- | xargs || true)
  [[ -n "$bg" || -n "$accent" ]] && echo "$theme_name  [$bg${accent:+ · $accent}]" || echo "$theme_name"
}

# -------- FIX: no reventar si se llama sin $1 --------
apply_theme() {
  local theme_name="${1-}"
  [[ -z "${theme_name}" ]] && { echo "Error: no theme provided" >&2; return 1; }

  local theme_file="$THEMES_DIR/${theme_name}.ini"

  if [[ "$theme_name" != "default" ]]; then
    [[ ! -f "$theme_file" ]] && { echo "Error: Theme file not found: $theme_file" >&2; return 1; }
    cp -f -- "$theme_file" "$COLORS_FILE"
  fi

  echo "$theme_name" > "$CURRENT_THEME_FILE"
  restart_polybar
}

restart_polybar() {
  if command -v polybar-msg &>/dev/null; then
    polybar-msg cmd restart &>/dev/null || true
  else
    pkill -x polybar || true
    sleep 0.3
    if [[ -f "$POLYBAR_DIR/launch.sh" ]]; then
      nohup "$POLYBAR_DIR/launch.sh" &>/dev/null &
    else
      nohup polybar toph &>/dev/null &
    fi
  fi
}

show_theme_selector() {
  local -a themes=()
  mapfile -t themes < <(get_available_themes)

  local current_theme; current_theme="$(get_current_theme)"
  if [[ ${#themes[@]} -eq 0 ]]; then
    notify "Error" "No themes available" -u critical
    echo "No themes available" >&2
    exit 1
  fi

  local list=""
  for t in "${themes[@]}"; do
    local info; info="$(get_theme_info "$t")"
    if [[ "$t" == "$current_theme" ]]; then
      list+="● $info"$'\n'
    else
      list+="  $info"$'\n'
    fi
  done

  local -a rofi_args=(-dmenu -i -p "Polybar Theme:" -no-custom)
  [[ -n "${ROFI_THEME:-}" ]] && rofi_args+=(-theme "$ROFI_THEME")

  local selected
  selected="$(printf '%s' "$list" | rofi "${rofi_args[@]}")" || exit 0
  [[ -z "$selected" ]] && exit 0

  # toma la PRIMERA palabra después de quitar los bullets/prefijos
  local theme_name
  theme_name="$(sed -E 's/^[● ]+//' <<<"$selected" | awk '{print $1}')"

  if apply_theme "$theme_name"; then
    notify "󰏘 Theme Applied" "Polybar: $theme_name" -t 2500 -i "preferences-desktop-theme"
    echo "Theme applied: $theme_name"
  else
    notify "󰀪 Error" "Could not apply: $theme_name" -u critical -t 4000
    echo "Error applying theme: $theme_name" >&2
    exit 1
  fi
}

quick_switch() {
  local -a themes=()
  mapfile -t themes < <(get_available_themes)
  local current_theme; current_theme="$(get_current_theme)"
  [[ ${#themes[@]} -eq 0 ]] && { echo "No themes available"; exit 1; }

  local idx=0
  for i in "${!themes[@]}"; do
    [[ "${themes[$i]}" == "$current_theme" ]] && { idx=$i; break; }
  done

  local next_idx=$(( (idx + 1) % ${#themes[@]} ))
  local next="${themes[$next_idx]}"
  if apply_theme "$next"; then
    notify "󰏘 Theme Changed" "$next" -t 2000
    echo "Changed to: $next"
  fi
}

list_themes() {
  local -a themes=()
  mapfile -t themes < <(get_available_themes)
  local current_theme; current_theme="$(get_current_theme)"

  echo "Available themes in $THEMES_DIR:"
  echo "=================================="
  for t in "${themes[@]}"; do
    [[ "$t" == "$current_theme" ]] && echo "  → $t (current)" || echo "    $t"
    local tf="$THEMES_DIR/${t}.ini"
    if [[ -f "$tf" ]]; then
      local colors_count
      colors_count=$(grep -cE "^[a-zA-Z-]+\s*=" "$tf" 2>/dev/null || echo "0")
      echo "      Defined colors: $colors_count"
    fi
  done
}

show_help() {
  cat <<EOF
Usage: $0 [OPTION]

Options:
  (no args) | rofi | selector   Show Rofi selector (usa tu tema por defecto)
  quick | next                  Cambiar rápidamente al siguiente tema
  current                       Mostrar tema actual
  list                          Listar temas disponibles
  help                          Mostrar esta ayuda

Notas:
- No se fuerza ningún tema de Rofi. Si exportas ROFI_THEME, se respetará.
- Los temas de Polybar se buscan en: $THEMES_DIR
EOF
}

case "${1:-rofi}" in
  ""|rofi|selector) show_theme_selector ;;
  quick|next)       quick_switch ;;
  current)          echo "Current theme: $(get_current_theme)" ;;
  list)             list_themes ;;
  help|-h|--help)   show_help ;;
  *) echo "Unrecognized option: $1"; show_help; exit 1 ;;
esac
