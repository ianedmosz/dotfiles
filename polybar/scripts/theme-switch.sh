#!/bin/bash
# ~/.config/polybar/scripts/theme-switch.sh
# Script to change Polybar themes using Rofi

POLYBAR_DIR="$HOME/.config/polybar"
THEMES_DIR="$POLYBAR_DIR/themes"
COLORS_FILE="$POLYBAR_DIR/colors.ini"
CURRENT_THEME_FILE="$POLYBAR_DIR/.current_theme"

# Verify that themes directory exists
if [[ ! -d "$THEMES_DIR" ]]; then
    notify-send "Error" "Themes directory not found: $THEMES_DIR" -u critical
    exit 1
fi

# Function to get available themes
get_available_themes() {
    local themes=()
    
    # Search for .ini files in themes directory
    if [[ -d "$THEMES_DIR" ]]; then
        for theme_file in "$THEMES_DIR"/*.ini; do
            if [[ -f "$theme_file" ]]; then
                # Get name without extension
                local theme_name=$(basename "$theme_file" .ini)
                themes+=("$theme_name")
            fi
        done
    fi
    
    # If no themes found, use colors.ini as default
    if [[ ${#themes[@]} -eq 0 ]]; then
        themes=("default")
    fi
    
    printf '%s\n' "${themes[@]}" | sort
}

# Function to get current theme
get_current_theme() {
    if [[ -f "$CURRENT_THEME_FILE" ]]; then
        cat "$CURRENT_THEME_FILE"
    else
        echo "default"
    fi
}

# Function to get theme color information
get_theme_info() {
    local theme_name="$1"
    local theme_file="$THEMES_DIR/${theme_name}.ini"
    
    if [[ ! -f "$theme_file" ]]; then
        echo "$theme_name (file not found)"
        return
    fi
    
    # Extract main colors to show info
    local bg=$(grep -E "^background\s*=" "$theme_file" | head -1 | cut -d'=' -f2 | tr -d ' ')
    local fg=$(grep -E "^foreground\s*=" "$theme_file" | head -1 | cut -d'=' -f2 | tr -d ' ')
    local accent=$(grep -E "^accent\s*=" "$theme_file" | head -1 | cut -d'=' -f2 | tr -d ' ')
    
    local info="$theme_name"
    if [[ -n "$bg" ]]; then
        info="$info  󰏘 $bg"
    fi
    if [[ -n "$accent" ]]; then
        info="$info  󰀫 $accent"
    fi
    
    echo "$info"
}

# Function to apply a theme
apply_theme() {
    local theme_name="$1"
    local theme_file="$THEMES_DIR/${theme_name}.ini"
    
    # Verify theme file exists
    if [[ ! -f "$theme_file" ]]; then
        echo "Error: Theme file not found: $theme_file" >&2
        return 1
    fi
    
    # Copy theme to colors.ini
    cp "$theme_file" "$COLORS_FILE"
    
    # Save current theme
    echo "$theme_name" > "$CURRENT_THEME_FILE"
    
    # Restart polybar
    restart_polybar
    
    return 0
}

# Function to restart polybar
restart_polybar() {
    if command -v polybar-msg &> /dev/null; then
        polybar-msg cmd restart &>/dev/null
    else
        # Alternative method
        pkill -f polybar
        sleep 0.5
        
        # Use launch.sh script if it exists
        if [[ -f "$POLYBAR_DIR/launch.sh" ]]; then
            nohup "$POLYBAR_DIR/launch.sh" &>/dev/null &
        else
            # Fallback: launch directly
            nohup polybar toph &>/dev/null &
        fi
    fi
}

# Function to show selector with Rofi
show_theme_selector() {
    local themes=($(get_available_themes))
    local current_theme=$(get_current_theme)
    
    if [[ ${#themes[@]} -eq 0 ]]; then
        notify-send "Error" "No themes available" -u critical
        exit 1
    fi
    
    # Create list for Rofi with additional information
    local theme_list=""
    for theme in "${themes[@]}"; do
        local theme_info=$(get_theme_info "$theme")
        if [[ "$theme" == "$current_theme" ]]; then
            theme_list="$theme_list● $theme_info\n"
        else
            theme_list="$theme_list  $theme_info\n"
        fi
    done
    
    # Style configuration for Rofi
    local rofi_theme="
    window {
        width: 500px;
        background-color: #1e1e2e;
        border: 2px;
        border-color: #89b4fa;
        border-radius: 8px;
    }
    listview {
        lines: 6;
        scrollbar: false;
    }
    element {
        padding: 8px 12px;
        border-radius: 4px;
        text-color: #ffffff;
        background-color: transparent;
    }
    element selected {
        background-color: #89b4fa;
        text-color: #000000;
    }
    element normal {
        text-color: #ffffff;
    }
    inputbar {
        padding: 8px;
        background-color: #313244;
        border-radius: 4px;
        margin: 8px;
    }
    prompt {
        text-color: #ffffff;
    }
    entry {
        text-color: #ffffff;
    }
    message {
        text-color: #ffffff;
    }
    "
    
    # Show Rofi
    local selected
    selected=$(echo -e "$theme_list" | rofi \
        -dmenu \
        -i \
        -p "󰏘 Polybar Theme:" \
        -mesg "<span color='#ffffff'>Current: $current_theme  |  Total: ${#themes[@]} themes</span>" \
        -theme-str "$rofi_theme" \
        -format "s" \
        -no-custom \
        -markup-rows)
    
    # If cancelled
    if [[ -z "$selected" ]]; then
        exit 0
    fi
    
    # Extract theme name (remove ● indicator and extra info)
    local theme_name=$(echo "$selected" | sed -E 's/^[● ]+([^ ]+).*/\1/')
    
    # Apply theme
    if apply_theme "$theme_name"; then
        if command -v notify-send &> /dev/null; then
            notify-send "󰏘 Theme Applied" "Polybar: $theme_name" \
                -t 2500 \
                -i "preferences-desktop-theme"
        fi
        echo "Theme applied: $theme_name"
    else
        if command -v notify-send &> /dev/null; then
            notify-send "󰀪 Error" "Could not apply: $theme_name" \
                -u critical \
                -t 4000
        fi
        echo "Error applying theme: $theme_name" >&2
    fi
}

# Function for quick switch to next theme
quick_switch() {
    local themes=($(get_available_themes))
    local current_theme=$(get_current_theme)
    
    if [[ ${#themes[@]} -eq 0 ]]; then
        echo "No themes available"
        exit 1
    fi
    
    # Find current theme index
    local current_index=0
    for i in "${!themes[@]}"; do
        if [[ "${themes[i]}" == "$current_theme" ]]; then
            current_index=$i
            break
        fi
    done
    
    # Calculate next theme
    local next_index=$(( (current_index + 1) % ${#themes[@]} ))
    local next_theme="${themes[next_index]}"
    
    # Apply next theme
    if apply_theme "$next_theme"; then
        if command -v notify-send &> /dev/null; then
            notify-send "󰏘 Theme Changed" "$next_theme" -t 2000
        fi
        echo "Changed to: $next_theme"
    fi
}

# Function to list available themes
list_themes() {
    local themes=($(get_available_themes))
    local current_theme=$(get_current_theme)
    
    echo "Available themes in $THEMES_DIR:"
    echo "=================================="
    
    for theme in "${themes[@]}"; do
        local theme_file="$THEMES_DIR/${theme}.ini"
        if [[ "$theme" == "$current_theme" ]]; then
            echo "  → $theme (current)"
        else
            echo "    $theme"
        fi
        
        # Show additional information if file exists
        if [[ -f "$theme_file" ]]; then
            local colors_count=$(grep -cE "^[a-zA-Z-]+\s*=" "$theme_file" 2>/dev/null || echo "0")
            echo "      Defined colors: $colors_count"
        fi
    done
}

# Help function
show_help() {
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  (no arguments)    Show Rofi selector"
    echo "  rofi              Show Rofi selector"
    echo "  quick             Quick switch to next theme"
    echo "  current           Show current theme"
    echo "  list              List all available themes"
    echo "  help              Show this help"
    echo ""
    echo "Theme files located in: $THEMES_DIR"
}

# Process arguments
case "${1:-rofi}" in
    ""|"rofi"|"selector")
        show_theme_selector
        ;;
    "quick"|"next")
        quick_switch
        ;;
    "current")
        echo "Current theme: $(get_current_theme)"
        ;;
    "list")
        list_themes
        ;;
    "help"|"-h"|"--help")
        show_help
        ;;
    *)
        echo "Unrecognized option: $1"
        show_help
        exit 1
        ;;
esac
