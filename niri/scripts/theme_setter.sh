#!/bin/bash
# Niri Theme Setter Script (KDL File version)

# --- Configuration ---
THEMES_DIR="$HOME/.config/niri/themes"
NIRI_CONFIG="$HOME/.config/niri/config.kdl"

# Define markers in config.kdl that this script will target for replacement
START_MARKER="// >>> THEME_START"
END_MARKER="// <<< THEME_END"

# --- Main Logic ---

# 1. Get available theme file names (without the .kdl extension)
THEME_LIST=$(find "$THEMES_DIR" -maxdepth 1 -type f -name "*.kdl" -printf "%f\n" | sed 's/\.kdl$//g')

if [ -z "$THEME_LIST" ]; then
    echo "Niri Theme Error: No theme files found in $THEMES_DIR. Please create them there."
    exit 1
fi

# 2. Use Wofi to select a theme (instead of Fuzzel)
SELECTED_THEME_BASE=$(echo -e "$THEME_LIST" | wofi --dmenu --prompt "Select Theme: ")

if [ -z "$SELECTED_THEME_BASE" ]; then
    # User cancelled selection
    exit 0
fi

THEME_FILE="$THEMES_DIR/$SELECTED_THEME_BASE.kdl"

if [ ! -f "$THEME_FILE" ]; then
    echo "Niri Theme Error: Theme file not found: $THEME_FILE"
    exit 1
fi

# 3. Read the entire content of the selected KDL theme file
THEME_CONTENT=$(cat "$THEME_FILE")

# 4. Replace the content between the markers using awk
# This is more reliable than sed for this type of replacement
awk -v start="$START_MARKER" -v end="$END_MARKER" -v content="$THEME_CONTENT" '
    BEGIN { in_block=0 }
    $0 ~ start { 
        print $0
        print content
        in_block=1
        next
    }
    $0 ~ end { 
        print $0
        in_block=0
        next
    }
    !in_block { print }
' "$NIRI_CONFIG" > "${NIRI_CONFIG}.tmp"

# Check if the replacement was successful
if [ $? -eq 0 ]; then
    mv "${NIRI_CONFIG}.tmp" "$NIRI_CONFIG"
    echo "Niri Theme Changed: Theme set to $SELECTED_THEME_BASE. Reloading Niri..."
    niri msg action load-config-file
else
    echo "Error: Failed to update config file"
    rm -f "${NIRI_CONFIG}.tmp"
    exit 1
fi

