#!/bin/bash

# Define the list of preferred media players (MPRIS IDs)
PREFERRED_PLAYERS="cider spotify"

# Define the maximum length for the output text before truncation
MAX_LENGTH=30

# Function to get the status and metadata for a specific player and output JSON
get_player_info() {
    local PLAYER_ID=$1
    
    local STATUS=$(playerctl --player="$PLAYER_ID" status 2>/dev/null)
    local PLAYER_STATUS=$?
    
    if [ $PLAYER_STATUS -eq 0 ]; then
        if [[ "$STATUS" == "Playing" || "$STATUS" == "Paused" ]]; then
            
            local METADATA=$(playerctl --player="$PLAYER_ID" metadata --format '{{ artist }} - {{ title }}' 2>/dev/null)
            
            # Filter out common browser junk titles
            if [[ "$METADATA" == *"Echo360"* || "$METADATA" == *"YouTube"* || "$METADATA" == *"Vimeo"* ]]; then
                return 1
            fi
            
            if [ -n "$METADATA" ]; then
                
                # --- TRUNCATION LOGIC STARTS HERE ---
                local DISPLAY_TEXT="$METADATA"
                if [ ${#METADATA} -gt $MAX_LENGTH ]; then
                    # Truncate and add ellipsis (...)
                    DISPLAY_TEXT="${METADATA:0:$MAX_LENGTH}..."
                fi
                # --- TRUNCATION LOGIC ENDS HERE ---
                
                # Output JSON object with the (potentially truncated) song name and status information
                # The full METADATA is still used for the "tooltip"
                echo "{\"text\": \"$DISPLAY_TEXT\", \"tooltip\": \"$METADATA ($PLAYER_ID $STATUS)\", \"alt\": \"$STATUS\"}"
                return 0
            fi
        fi
    fi
    return 1
}

ALL_PLAYERS=$(playerctl -l 2>/dev/null)

# 1. Loop through preferred desktop players first
for PLAYER in $PREFERRED_PLAYERS; do
    get_player_info "$PLAYER"
    if [ $? -eq 0 ]; then
        exit 0
    fi
done

# 2. If no preferred player is found, loop through ALL active players
for PLAYER in $ALL_PLAYERS; do
    if [[ "$PREFERRED_PLAYERS" != *"$PLAYER"* ]]; then
        get_player_info "$PLAYER"
        if [ $? -eq 0 ]; then
            exit 0
        fi
    fi
done

# Output nothing if no player is found
echo ""
