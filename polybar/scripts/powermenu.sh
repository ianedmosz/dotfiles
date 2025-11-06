
#!/usr/bin/env bash
# Rofi Powermenu — uses your default rofi config/theme (no hardcoded theme).
# Supports Wayland/X11. Tries swaylock/hyprlock/i3lock for lock. WM-aware logout.

set -euo pipefail

# -------- Helpers --------
have() { command -v "$1" >/dev/null 2>&1; }

# Pick a lock command
pick_lock() {
  if have swaylock; then echo "swaylock -f"; return; fi
  if have hyprlock; then echo "hyprlock"; return; fi
  if have i3lock; then echo "i3lock -c 000000"; return; fi
  # systemd-based lock if nothing else
  if have loginctl; then echo "loginctl lock-session"; return; fi
  echo ""  # no lock available
}

# Pick a logout command (tries to detect WM/DE)
pick_logout() {
  if have hyprctl && pgrep -x Hyprland >/dev/null 2>&1; then echo "hyprctl dispatch exit"; return; fi
  if have swaymsg && pgrep -x sway >/dev/null 2>&1; then echo "swaymsg exit"; return; fi
  if have i3-msg && pgrep -x i3 >/dev/null 2>&1; then echo "i3-msg exit"; return; fi
  if have bspc && pgrep -x bspwm >/dev/null 2>&1; then echo "bspc quit"; return; fi
  # Generic: try to end the session
  if have loginctl; then echo "loginctl terminate-session ${XDG_SESSION_ID:-self}"; return; fi
  echo "pkill -KILL -u $USER"
}

LOCK_CMD="$(pick_lock)"
LOGOUT_CMD="$(pick_logout)"

# Entries (icon + label). Change labels if you want Spanish/English/etc.
# Icons assume a Nerd Font / Font Awesome pack; adjust if needed.
entries=()
[ -n "$LOCK_CMD" ] && entries+=("  Lock")
entries+=("  Sleep" "  Reboot" "  Shutdown" "  Logout" )

# Build rofi menu input
menu_input=$(printf '%s\n' "${entries[@]}")

# Show menu (uses your default rofi config & theme)
choice="$(echo "$menu_input" | rofi -dmenu -p "Power" -i -no-fixed-num-lines)"
[ -z "${choice}" ] && exit 0

# Optional confirmation for destructive actions
confirm() {
  local prompt="$1"
  local sel
  sel="$(printf "No\nYes\n" | rofi -dmenu -p "$prompt" -i -no-fixed-num-lines)"
  [ "$sel" = "Yes" ]
}

do_action() {
  case "$choice" in
    "  Lock")
      [ -n "$LOCK_CMD" ] && eval "$LOCK_CMD" &
      ;;
    "  Sleep")
      confirm "Sleep?" && systemctl suspend
      ;;
    "  Reboot")
      confirm "Reboot?" && systemctl reboot
      ;;
    "  Shutdown")
      confirm "Power off?" && systemctl poweroff
      ;;
    "  Logout")
      confirm "Logout?" && eval "$LOGOUT_CMD"
      ;;
  esac
}

do_action
