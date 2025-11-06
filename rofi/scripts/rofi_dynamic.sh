#!/bin/bash

# --- 1. Define Paths ---

# File where the current wallpaper path is stored (e.g., /home/user/pictures/new.jpg)
PERSISTENCE_FILE="$HOME/.config/niri/last_wallpaper.txt"

# Your Rofi theme file where the 'inputbar' is defined.
# ASSUMPTION: This is the file containing the 'inputbar' block you pasted.
ROFI_THEME_FILE="$HOME/.config/rofi/config.rasi" # Adjust this path if your Rofi config is elsewhere!

# --- 2. Read and Validate Wallpaper Path ---
if [[ ! -f "$PERSISTENCE_FILE" ]]; then
    echo "ERROR: Persistence file not found at $PERSISTENCE_FILE"
    exit 1
fi

NEW_WALLPAPER_PATH=$(head -n1 "$PERSISTENCE_FILE")

if [[ -z "$NEW_WALLPAPER_PATH" || ! -f "$NEW_WALLPAPER_PATH" ]]; then
    echo "WARNING: New wallpaper file not found or path is empty. Using old path as fallback."
    # The script will proceed using the potentially broken path, letting Rofi handle the fallback.
fi

# --- 3. Construct the Replacement Line ---

# The line we are injecting into the .rasi file
# IMPORTANT: The path must be enclosed in double quotes for Rofi (e.g., "url(\"/path/file.jpg\", width);")
REPLACEMENT_LINE="background-image: url(\"$NEW_WALLPAPER_PATH\", width);"

# --- 4. Permanent Replacement using sed ---

if [[ ! -f "$ROFI_THEME_FILE" ]]; then
    echo "ERROR: Rofi theme file not found at $ROFI_THEME_FILE. Cannot update."
    exit 1
fi

# Use sed to find the line 'background-image: ...' inside the 'inputbar' block and replace it.
# We use '#' as a delimiter for sed because the path contains '/', avoiding conflicts.
# The 's' command performs the substitution globally within the inputbar block.

sed -i "/inputbar {/,/}/ s|^[[:space:]]*background-image:.*|$REPLACEMENT_LINE|" "$ROFI_THEME_FILE"

echo "SUCCESS: Rofi theme updated. Inputbar now points to: $NEW_WALLPAPER_PATH"

exit 0
