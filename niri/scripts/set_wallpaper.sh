#!/bin/bash

# --- Configuration ---
WALLPAPER_DIR="$HOME/.config/wallpapers"
PERSISTENCE_FILE="$HOME/.config/niri/last_wallpaper.txt"
SWAYBG_MODE="fill"
WOFI_PROMPT="Select Wallpaper:"

# --- Ensure persistence dir exists ---
mkdir -p "$(dirname "$PERSISTENCE_FILE")"

# --- Find wallpapers ---
wallpaper_list=$(find "$WALLPAPER_DIR" -maxdepth 1 -type f \
    \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) \
    -exec basename {} \; 2>/dev/null)

if [[ -z "$wallpaper_list" ]]; then
    notify-send -u critical "Wallpaper Error" "No wallpapers found in $WALLPAPER_DIR"
    exit 1
fi

# --- Select wallpaper ---
SELECTED_FILE=$(echo -e "$wallpaper_list" | wofi --dmenu --prompt "$WOFI_PROMPT")

if [[ -z "$SELECTED_FILE" ]]; then
    # Si no eliges nada pero ya hay un archivo, reusa el último
    if [[ -f "$PERSISTENCE_FILE" ]]; then
        IMG=$(head -n1 "$PERSISTENCE_FILE")
        MODE=$(tail -n1 "$PERSISTENCE_FILE")
        swaybg -m "$MODE" -i "$IMG" &
        notify-send "Wallpaper Loader" "Se aplicó el último wallpaper guardado."
    else
        notify-send -u critical "Wallpaper Error" "No wallpaper selected and no persistence file found."
    fi
    exit 0
fi

WALLPAPER_PATH="$WALLPAPER_DIR/$SELECTED_FILE"

# --- Apply wallpaper ---
swaybg -m "$SWAYBG_MODE" -i "$WALLPAPER_PATH" &

# --- Save wallpaper + mode ---
echo "$WALLPAPER_PATH" > "$PERSISTENCE_FILE"
echo "$SWAYBG_MODE" >> "$PERSISTENCE_FILE"

notify-send "Wallpaper Updated" "Set: $SELECTED_FILE"

