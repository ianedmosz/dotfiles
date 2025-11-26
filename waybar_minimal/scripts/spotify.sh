#!/bin/bash

PLAYER_ID="spotify"

# --- Fetch Metadata and Status in one go ---

# Use --format to combine the Artist and Title. 
# This template often handles multi-artist fields better.
# If the artist is still truncated, you might need a more complex solution, 
# but this is the standard fix for Spotify issues.
METADATA_LINE=$(playerctl --player="$PLAYER_ID" metadata --format '{{ artist }} - {{ title }}' 2>/dev/null)

# Check if Spotify is running/active (check if the line is empty)
if [ -z "$METADATA_LINE" ]; then
    notify-send "Spotify Control" "Spotify player not found or no song is currently playing."
    exit 0
fi

# Set the Wofi command with the dynamic prompt (the full song info)
MENU_CMD="wofi -dmenu -i -p '$METADATA_LINE'"

# --- Determine Play/Pause state and icon ---
STATUS=$(playerctl --player="$PLAYER_ID" status 2>/dev/null)

if [ "$STATUS" = "Playing" ]; then
    #  is the Pause glyph (Nerd Font)
    PLAY_PAUSE_LABEL=" Pause"
else
    #  is the Play/Resume glyph (Nerd Font)
    PLAY_PAUSE_LABEL=" Play"
fi

# Define the menu options
#  = Next,  = Previous (Nerd Fonts)
MENU_OPTIONS="$PLAY_PAUSE_LABEL\n Next\n Previous"

# --- Launch Wofi and get selection ---
SELECTION=$(echo -e "$MENU_OPTIONS" | $MENU_CMD)

# --- Execute command based on selection ---
case "$SELECTION" in
    " Pause" | " Play")
        playerctl --player="$PLAYER_ID" play-pause
        ;;
    " Next")
        playerctl --player="$PLAYER_ID" next
        ;;
    " Previous")
        playerctl --player="$PLAYER_ID" previous
        ;;
    *)
        # Do nothing if user escapes or clicks outside
        exit 0
        ;;
esac

exit 0
