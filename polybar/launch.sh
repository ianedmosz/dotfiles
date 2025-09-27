#!/bin/bash
# ~/.config/polybar/launch.sh

# Directories and files
POLYBAR_DIR="$HOME/.config/polybar"
THEMES_DIR="$POLYBAR_DIR/themes"
COLORS_FILE="$POLYBAR_DIR/colors.ini"
CURRENT_THEME_FILE="$POLYBAR_DIR/.current_theme"

# Function to restore saved theme
restore_theme() {
    if [[ -f "$CURRENT_THEME_FILE" ]]; then
        local saved_theme=$(cat "$CURRENT_THEME_FILE")
        local theme_file="$THEMES_DIR/${saved_theme}.ini"
        
        if [[ -f "$theme_file" ]]; then
            echo "Restoring theme: $saved_theme"
            cp "$theme_file" "$COLORS_FILE"
        else
            echo "Warning: Saved theme file not found: $theme_file"
            # Use default if saved theme doesn't exist
            if [[ -f "$THEMES_DIR/default.ini" ]]; then
                cp "$THEMES_DIR/default.ini" "$COLORS_FILE"
                echo "default" > "$CURRENT_THEME_FILE"
            fi
        fi
    else
        echo "No saved theme found, using default"
        # Create default theme file if it doesn't exist
        if [[ -f "$THEMES_DIR/default.ini" ]]; then
            cp "$THEMES_DIR/default.ini" "$COLORS_FILE"
            echo "default" > "$CURRENT_THEME_FILE"
        fi
    fi
}

# Terminate already running Polybar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Restore the saved theme before launching
restore_theme

# Launch Polybar (adjust the bar name as needed)
echo "---" | tee -a /tmp/polybar1.log
polybar toph 2>&1 | tee -a /tmp/polybar1.log & disown

echo "Polybar launched with restored theme"
