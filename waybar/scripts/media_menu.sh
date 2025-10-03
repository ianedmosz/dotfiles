#!/bin/bash

# Define the list of preferred media players (MPRIS IDs)
PREFERRED_PLAYERS="spotify cider"

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
                # Output JSON object with the song name and status information
                echo "{\"text\": \"$METADATA\", \"tooltip\": \"$PLAYER_ID ($STATUS)\", \"class\": \"media-player\"}"
                return 0
            fi
        fi
    fi
    return 1
}

ALL_PLAYERS=$(playerctl -l 2>/dev/null)

# 1. Loop through preferred desktop players first (Spotify is priority 1)
for PLAYER in $PREFERRED_PLAYERS; do
    get_player_info "$PLAYER"
    if [ $? -eq 0 ]; then
        exit 0
    fi
done

# 2. If no preferred player is found, loop through all others (like browsers)
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
