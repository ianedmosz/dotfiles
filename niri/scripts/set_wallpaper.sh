#!/bin/bash

# --- Configuration ---
# The location of your wallpapers
WALLPAPER_DIR="$HOME/.config/wallpapers"
# Persistence file location (where the last path is saved)
PERSISTENCE_FILE="$HOME/.config/fuzzel/last_wallpaper.txt"
# Swaybg scaling mode (used both for applying and saving)
SWAYBG_MODE="fill"
WOFI_PROMPT="Select Wallpaper:"

# --- Main Logic ---

# 1. Find and list available wallpapers
wallpaper_list=$(find "$WALLPAPER_DIR" -maxdepth 1 -type f \
    \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) \
    -exec basename {} \; 2>/dev/null)

if [[ -z "$wallpaper_list" ]]; then
    notify-send -u critical "Wallpaper Error" "No wallpapers found in $WALLPAPER_DIR"
    exit 1
fi

# 2. Pipe the list to Wofi for user selection
SELECTED_FILE=$(echo -e "$wallpaper_list" | wofi --dmenu --prompt "$WOFI_PROMPT")

if [[ -z "$SELECTED_FILE" ]]; then
    exit 0 # Selection cancelled
fi

WALLPAPER_PATH="$WALLPAPER_DIR/$SELECTED_FILE"

# 3. Apply the wallpaper immediately
swaybg -m "$SWAYBG_MODE" -i "$WALLPAPER_PATH"

# 4. Save the path and mode to the persistence file
# This is what the Niri startup script will read later.
mkdir -p "$(dirname "$PERSISTENCE_FILE")"
echo "$WALLPAPER_PATH" > "$PERSISTENCE_FILE"
echo "$SWAYBG_MODE" >> "$PERSISTENCE_FILE"

notify-send "Wallpaper Updated" "Set: $SELECTED_FILE"

