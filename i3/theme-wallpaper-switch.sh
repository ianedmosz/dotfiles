#!/bin/bash
# ~/.config/i3/scripts/theme-wallpaper-switch.sh
# Script to change i3 themes and wallpapers using Rofi

I3_DIR="$HOME/.config/i3"
THEMES_DIR="$I3_DIR/themes"
WALLPAPERS_DIR="$HOME/.config/wallpapers"
CONFIG_FILE="$I3_DIR/config"
CURRENT_THEME_FILE="$I3_DIR/.current_theme"
CURRENT_WALLPAPER_FILE="$I3_DIR/.current_wallpaper"
CONFIG_BASE="$I3_DIR/config.base"

# Create directories if they don't exist
mkdir -p "$THEMES_DIR"
mkdir -p "$WALLPAPERS_DIR"

# Verify that base config exists
if [[ ! -f "$CONFIG_BASE" ]]; then
    echo "Warning: Base config file not found: $CONFIG_BASE"
    echo "Creating backup of current config as base..."
    cp "$CONFIG_FILE" "$CONFIG_BASE" 2>/dev/null || {
        echo "Error: Could not create base config file"
        exit 1
    }
fi

# Function to create example themes if none exist
create_example_themes() {
    if [[ $(find "$THEMES_DIR" -name "*.theme" 2>/dev/null | wc -l) -gt 0 ]]; then
        return 0  # Themes already exist
    fi

    echo "Creating example theme files..."
    
    # Create a README file
    cat > "$THEMES_DIR/README.md" << 'EOF'
# i3 Theme Files

Each .theme file should contain:
1. Color variables (optional)
2. client.* color definitions
3. Bar colors configuration
4. Wallpaper preference (optional comment)

## Example structure:
```
# Theme name: example
# Optional color variables
set $bg #282a36
set $fg #f8f8f2

# Window colors
client.focused          #color1 #color2 #color3 #color4 #color5
client.focused_inactive #color1 #color2 #color3 #color4 #color5
client.unfocused        #color1 #color2 #color3 #color4 #color5
client.urgent           #color1 #color2 #color3 #color4 #color5
client.placeholder      #color1 #color2 #color3 #color4 #color5
client.background       #color

# Bar colors (if using i3bar)
bar {
    colors {
        background #color
        statusline #color
        separator  #color
        
        focused_workspace  #color1 #color2 #color3
        active_workspace   #color1 #color2 #color3
        inactive_workspace #color1 #color2 #color3
        urgent_workspace   #color1 #color2 #color3
        binding_mode       #color1 #color2 #color3
    }
}

# wallpaper: preferred_wallpaper.jpg (optional)
```
EOF

    echo "Theme files should be created in: $THEMES_DIR"
    echo "Example theme structure saved in: $THEMES_DIR/README.md"
}

# Function to get available themes
get_available_themes() {
    local themes=()
    
    # Search for .theme files in themes directory
    if [[ -d "$THEMES_DIR" ]]; then
        for theme_file in "$THEMES_DIR"/*.theme; do
            if [[ -f "$theme_file" ]]; then
                # Get name without extension
                local theme_name=$(basename "$theme_file" .theme)
                themes+=("$theme_name")
            fi
        done
    fi
    
    # If no themes found, create examples info
    if [[ ${#themes[@]} -eq 0 ]]; then
        create_example_themes
        echo "No theme files found. Please create .theme files in $THEMES_DIR" >&2
        exit 1
    fi
    
    printf '%s\n' "${themes[@]}" | sort
}

# Function to get available wallpapers
get_available_wallpapers() {
    local wallpapers=()
    
    # Look for common image formats
    for wallpaper in "$WALLPAPERS_DIR"/*.{jpg,jpeg,png,webp,bmp,tiff} "$WALLPAPERS_DIR"/*/*.{jpg,jpeg,png,webp,bmp,tiff}; do
        if [[ -f "$wallpaper" ]]; then
            # Get relative path from wallpapers directory
            local relative_path="${wallpaper#$WALLPAPERS_DIR/}"
            wallpapers+=("$relative_path")
        fi
    done
    
    # Remove duplicates and sort
    printf '%s\n' "${wallpapers[@]}" | sort -u
}

# Function to get theme wallpapers (theme-specific + all available)
get_theme_wallpapers() {
    local theme_name="$1"
    local wallpapers=()
    
    # First, look for theme-specific wallpapers
    for wallpaper in "$WALLPAPERS_DIR"/${theme_name}*.{jpg,jpeg,png,webp,bmp,tiff} "$WALLPAPERS_DIR"/${theme_name}_*.{jpg,jpeg,png,webp,bmp,tiff}; do
        if [[ -f "$wallpaper" ]]; then
            local relative_path="${wallpaper#$WALLPAPERS_DIR/}"
            wallpapers+=("$relative_path")
        fi
    done
    
    # Then add all other available wallpapers
    local all_wallpapers=($(get_available_wallpapers))
    for wallpaper in "${all_wallpapers[@]}"; do
        # Add if not already in theme-specific list
        if [[ ! " ${wallpapers[*]} " =~ " ${wallpaper} " ]]; then
            wallpapers+=("$wallpaper")
        fi
    done
    
    printf '%s\n' "${wallpapers[@]}"
}

# Function to get current theme
get_current_theme() {
    if [[ -f "$CURRENT_THEME_FILE" ]]; then
        cat "$CURRENT_THEME_FILE"
    else
        echo "none"
    fi
}

# Function to get current wallpaper
get_current_wallpaper() {
    if [[ -f "$CURRENT_WALLPAPER_FILE" ]]; then
        cat "$CURRENT_WALLPAPER_FILE"
    else
        echo "none"
    fi
}

# Function to get theme info with preview
get_theme_info() {
    local theme_name="$1"
    local is_current="$2"
    local theme_file="$THEMES_DIR/${theme_name}.theme"
    
    if [[ ! -f "$theme_file" ]]; then
        echo "$theme_name (file not found)"
        return
    fi
    
    # Extract main focused color and background
    local focused_bg=$(grep "client.focused" "$theme_file" | head -1 | awk '{print $3}')
    local bar_bg=$(grep -A 10 "bar {" "$theme_file" | grep "background" | head -1 | awk '{print $2}')
    local wallpaper_count=$(get_theme_wallpapers "$theme_name" | wc -l)
    
    # Extract theme description from comments
    local description=$(grep "^# Theme name:" "$theme_file" | sed 's/^# Theme name: //')
    
    # Create a more structured display
    local status_icon=""
    if [[ "$is_current" == "true" ]]; then
        status_icon="✓ "
    else
        status_icon="  "
    fi
    
    # Format theme name with description
    local display_name="$theme_name"
    if [[ -n "$description" ]]; then
        display_name="$theme_name - $description"
    fi
    
    # Create color preview (simplified)
    local color_preview=""
    if [[ -n "$focused_bg" ]]; then
        color_preview="  🎨 $focused_bg"
    fi
    
    # Wallpaper count
    local wallpaper_info=""
    if [[ $wallpaper_count -gt 0 ]]; then
        wallpaper_info="  🖼️ $wallpaper_count"
    else
        wallpaper_info="  🖼️ 0"
    fi
    
    echo "${status_icon}${display_name}${color_preview}${wallpaper_info}"
}

# Function to get wallpaper info
get_wallpaper_info() {
    local wallpaper_name="$1"
    local full_path="$WALLPAPERS_DIR/$wallpaper_name"
    
    if [[ ! -f "$full_path" ]]; then
        echo "$wallpaper_name (not found)"
        return
    fi
    
    # Get basic file info
    local size=""
    if command -v identify &> /dev/null; then
        size=$(identify -format "%wx%h" "$full_path" 2>/dev/null)
        if [[ -n "$size" ]]; then
            size=" 📐 $size"
        fi
    fi
    
    local file_size=$(du -h "$full_path" 2>/dev/null | cut -f1)
    
    echo "$wallpaper_name  📁 $file_size$size"
}

# Function to set wallpaper
set_wallpaper() {
    local wallpaper_file="$1"
    local full_path="$WALLPAPERS_DIR/$wallpaper_file"
    
    if [[ ! -f "$full_path" ]]; then
        echo "Warning: Wallpaper not found: $full_path" >&2
        return 1
    fi
    
    # Try different wallpaper setters in order of preference
    if command -v feh &> /dev/null; then
        feh --bg-fill "$full_path"
    elif command -v nitrogen &> /dev/null; then
        nitrogen --set-zoom-fill "$full_path"
    elif command -v xwallpaper &> /dev/null; then
        xwallpaper --zoom "$full_path"
    elif command -v hsetroot &> /dev/null; then
        hsetroot -fill "$full_path"
    elif command -v swaybg &> /dev/null && [[ "$XDG_SESSION_TYPE" == "wayland" ]]; then
        # For Wayland/sway users
        pkill swaybg 2>/dev/null
        swaybg -i "$full_path" -m fill &
    else
        echo "Warning: No wallpaper setter found (feh, nitrogen, xwallpaper, hsetroot, swaybg)" >&2
        return 1
    fi
    
    return 0
}

# Function to apply theme
apply_theme() {
    local theme_name="$1"
    local theme_file="$THEMES_DIR/${theme_name}.theme"
    
    # Verify theme file exists
    if [[ ! -f "$theme_file" ]]; then
        echo "Error: Theme file not found: $theme_file" >&2
        return 1
    fi
    
    # Create new config by combining base config with theme
    {
        # Add base config without existing theme lines
        grep -v -E "^(client\.|bar|set \$.*#|.*colors.*)" "$CONFIG_BASE" | \
        grep -v "# Theme colors applied by theme-wallpaper-switch.sh" | \
        grep -v "# Bar configuration from theme"
        
        echo ""
        echo "# Theme colors applied by theme-wallpaper-switch.sh"
        cat "$theme_file"
    } > "$CONFIG_FILE"
    
    # Save current theme
    echo "$theme_name" > "$CURRENT_THEME_FILE"
    
    # Reload i3 configuration
    i3-msg reload >/dev/null 2>&1
    
    return 0
}

# Function to restore theme without reloading (for startup)
restore_theme_startup() {
    local theme_name="$1"
    local theme_file="$THEMES_DIR/${theme_name}.theme"
    
    # Verify theme file exists
    if [[ ! -f "$theme_file" ]]; then
        echo "Warning: Theme file not found: $theme_file" >&2
        return 1
    fi
    
    # Create new config by combining base config with theme
    {
        # Add base config without existing theme lines
        grep -v -E "^(client\.|bar|set \$.*#|.*colors.*)" "$CONFIG_BASE" | \
        grep -v "# Theme colors applied by theme-wallpaper-switch.sh" | \
        grep -v "# Bar configuration from theme"
        
        echo ""
        echo "# Theme colors applied by theme-wallpaper-switch.sh"
        cat "$theme_file"
    } > "$CONFIG_FILE"
    
    echo "Theme restored: $theme_name"
    return 0
}

# Function to show theme selector
show_theme_selector() {
    local themes=($(get_available_themes))
    local current_theme=$(get_current_theme)
    
    if [[ ${#themes[@]} -eq 0 ]]; then
        notify-send "Error" "No themes available" -u critical
        exit 1
    fi
    
    # Create list for Rofi
    local theme_list=""
    for theme in "${themes[@]}"; do
        local is_current="false"
        if [[ "$theme" == "$current_theme" ]]; then
            is_current="true"
        fi
        local theme_info=$(get_theme_info "$theme" "$is_current")
        theme_list="$theme_list$theme_info\n"
    done
    
    # Rofi configuration for themes
    local rofi_theme="
    window {
        width: 800px;
        height: 600px;
        background-color: #1e1e2e;
        border: 2px;
        border-color: #89b4fa;
        border-radius: 12px;
    }
    listview {
        lines: 10;
        scrollbar: true;
    }
    element {
        padding: 16px 20px;
        border-radius: 8px;
        margin: 2px 8px;
        background-color: transparent;
    }
    element selected {
        background-color: #89b4fa;
        text-color: #1e1e2e;
        border-radius: 8px;
    }
    inputbar {
        padding: 16px;
        background-color: #313244;
        border-radius: 8px;
        margin: 16px;
    }
    prompt {
        text-color: #89b4fa;
    }
    entry {
        text-color: #cdd6f4;
    }
    message {
        padding: 12px;
        margin: 16px;
        background-color: #313244;
        border-radius: 8px;
        text-color: #89b4fa;
    }
    "
    
    # Show theme selector
    local selected
    selected=$(echo -e "$theme_list" | rofi \
        -dmenu \
        -i \
        -p "🎨 Select i3 Theme:" \
        -mesg "<span color='#f9e2af'>Current: $current_theme  •  ${#themes[@]} themes available  •  Next: wallpaper selection</span>" \
        -theme-str "$rofi_theme" \
        -format "s" \
        -no-custom \
        -markup-rows \
        -kb-custom-1 "Alt+1" \
        -kb-custom-2 "Alt+2")
    
    if [[ -z "$selected" ]]; then
        exit 0
    fi
    
    # Extract theme name (handle the new format with ✓)
    local theme_name=$(echo "$selected" | sed -E 's/^[✓ ]+([^ ]+).*/\1/')
    
    # Show wallpaper selector for chosen theme
    show_wallpaper_selector "$theme_name"
}

# Function to show wallpaper selector (similar style to theme selector)
show_wallpaper_selector() {
    local theme_name="$1"
    local wallpapers=($(get_theme_wallpapers "$theme_name"))
    local current_wallpaper=$(get_current_wallpaper)
    
    if [[ ${#wallpapers[@]} -eq 0 ]]; then
        # Apply theme without wallpaper
        if apply_theme "$theme_name"; then
            notify-send "🎨 Theme Applied" "i3: $theme_name (no wallpapers found)" -t 3000
        fi
        return
    fi
    
    # Create wallpaper list with info
    local wallpaper_list=""
    
    # Add option to apply theme only
    wallpaper_list="  [Apply theme only]  🎨 Apply $theme_name without changing wallpaper\n"
    
    # Add wallpapers with info
    for wallpaper in "${wallpapers[@]}"; do
        local wallpaper_info=$(get_wallpaper_info "$wallpaper")
        if [[ "$wallpaper" == "$current_wallpaper" ]]; then
            wallpaper_list="$wallpaper_list● $wallpaper_info\n"
        else
            wallpaper_list="$wallpaper_list  $wallpaper_info\n"
        fi
    done
    
    # Rofi configuration for wallpapers (similar to theme selector)
    local rofi_theme="
    window {
        width: 900px;
        height: 650px;
        background-color: #1e1e2e;
        border: 2px;
        border-color: #a6e3a1;
        border-radius: 12px;
    }
    listview {
        lines: 12;
        scrollbar: true;
    }
    element {
        padding: 14px 20px;
        border-radius: 8px;
        margin: 2px 8px;
        background-color: transparent;
    }
    element selected {
        background-color: #a6e3a1;
        text-color: #1e1e2e;
        border-radius: 8px;
    }
    inputbar {
        padding: 16px;
        background-color: #313244;
        border-radius: 8px;
        margin: 16px;
    }
    prompt {
        text-color: #a6e3a1;
    }
    entry {
        text-color: #cdd6f4;
    }
    message {
        padding: 12px;
        margin: 16px;
        background-color: #313244;
        border-radius: 8px;
        text-color: #a6e3a1;
    }
    "
    
    # Show wallpaper selector
    local selected_wallpaper
    selected_wallpaper=$(echo -e "$wallpaper_list" | rofi \
        -dmenu \
        -i \
        -p "🖼️ Select Wallpaper for '$theme_name':" \
        -mesg "<span color='#f9e2af'>Current wallpaper: $current_wallpaper  •  ${#wallpapers[@]} wallpapers available</span>" \
        -theme-str "$rofi_theme" \
        -format "s" \
        -no-custom \
        -markup-rows \
        -kb-custom-1 "Alt+1" \
        -kb-custom-2 "Alt+2")
    
    if [[ -z "$selected_wallpaper" ]]; then
        exit 0
    fi
    
    # Apply theme with or without wallpaper
    local wallpaper_name=""
    if [[ "$selected_wallpaper" != *"[Apply theme only]"* ]]; then
        # Extract wallpaper name (remove indicator and extra info)
        wallpaper_name=$(echo "$selected_wallpaper" | sed -E 's/^[✓ ]+([^ ]+).*/\1/')
    fi
    
    # Apply theme
    if apply_theme "$theme_name"; then
        local success=true
        local message="🎨 $theme_name"
        
        # Apply wallpaper if selected
        if [[ -n "$wallpaper_name" ]]; then
            if set_wallpaper "$wallpaper_name"; then
                echo "$wallpaper_name" > "$CURRENT_WALLPAPER_FILE"
                message="$message + 🖼️ $wallpaper_name"
            else
                success=false
                message="$message (wallpaper failed)"
            fi
        fi
        
        if [[ "$success" == "true" ]]; then
            notify-send "✅ Applied Successfully" "$message" -t 3000 -i "preferences-desktop-theme"
        else
            notify-send "⚠️ Partially Applied" "$message" -t 4000 -u normal
        fi
        
        echo "Theme applied: $theme_name"
        if [[ -n "$wallpaper_name" ]]; then
            echo "Wallpaper set: $wallpaper_name"
        fi
    else
        notify-send "❌ Error" "Could not apply theme: $theme_name" -u critical -t 4000
        echo "Error applying theme: $theme_name" >&2
    fi
}

# Function for quick switch
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
    
    # Apply next theme with first available wallpaper
    local wallpapers=($(get_theme_wallpapers "$next_theme"))
    local wallpaper=""
    if [[ ${#wallpapers[@]} -gt 0 ]]; then
        wallpaper="${wallpapers[0]}"
    fi
    
    if apply_theme "$next_theme"; then
        local message="🎨 $next_theme"
        
        if [[ -n "$wallpaper" ]] && set_wallpaper "$wallpaper"; then
            echo "$wallpaper" > "$CURRENT_WALLPAPER_FILE"
            message="$message + 🖼️ $wallpaper"
        fi
        
        notify-send "🔄 Quick Switch" "$message" -t 2000
        echo "Switched to: $next_theme"
    fi
}

# Function to list themes and wallpapers
list_themes() {
    local themes=($(get_available_themes))
    local current_theme=$(get_current_theme)
    local current_wallpaper=$(get_current_wallpaper)
    
    echo "Available themes in $THEMES_DIR:"
    echo "================================================"
    
    for theme in "${themes[@]}"; do
        local theme_file="$THEMES_DIR/${theme}.theme"
        if [[ "$theme" == "$current_theme" ]]; then
            echo "  → $theme (current)"
        else
            echo "    $theme"
        fi
        
        if [[ -f "$theme_file" ]]; then
            local wallpapers=($(get_theme_wallpapers "$theme"))
            echo "      Available wallpapers (${#wallpapers[@]}): ${wallpapers[*]:0:3}..."
        fi
    done
    
    echo ""
    echo "Current status:"
    echo "  Theme: $current_theme"
    echo "  Wallpaper: $current_wallpaper"
    echo ""
    echo "Directories:"
    echo "  Themes: $THEMES_DIR"
    echo "  Wallpapers: $WALLPAPERS_DIR"
}

# Help function
show_help() {
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  (no arguments)    Show theme and wallpaper selector"
    echo "  selector          Show theme and wallpaper selector"
    echo "  quick             Quick switch to next theme"
    echo "  current           Show current theme and wallpaper"
    echo "  list              List all available themes and wallpapers"
    echo "  restore_theme     Restore specific theme (for startup)"
    echo "  help              Show this help"
    echo ""
    echo "Directories:"
    echo "  Themes:     $THEMES_DIR (*.theme files)"
    echo "  Wallpapers: $WALLPAPERS_DIR (image files)"
    echo ""
    echo "Theme file format: Place .theme files with i3 color definitions"
    echo "Wallpaper naming: Any image format, theme-specific names preferred"
}

# Process arguments
case "${1:-selector}" in
    ""|"selector"|"rofi")
        show_theme_selector
        ;;
    "quick"|"next")
        quick_switch
        ;;
    "current")
        echo "Current theme: $(get_current_theme)"
        echo "Current wallpaper: $(get_current_wallpaper)"
        ;;
    "list")
        list_themes
        ;;
    "restore_theme")
        if [[ -n "$2" ]]; then
            restore_theme_startup "$2"
        else
            echo "Usage: $0 restore_theme <theme_name>"
            exit 1
        fi
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
