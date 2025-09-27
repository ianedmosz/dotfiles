#!/bin/bash
# ── volume-pamixer.sh ─────────────────────────────────────────────
# Description: Super fast volume script using pamixer
# Usage: Polybar custom module with exec
# Dependencies: pamixer
# ──────────────────────────────────────────────────────────────────

# Obtener volumen y estado mute con pamixer (súper rápido)
vol_int=$(pamixer --get-volume)
is_muted=$(pamixer --get-mute)

# Icon logic
if [ "$is_muted" = "true" ]; then
    icon="󰝟"
    display_vol=0
else
    display_vol="$vol_int"
    if [ "$vol_int" -eq 0 ]; then
        icon="󰕿"
    elif [ "$vol_int" -lt 50 ]; then
        icon="󰖀"
    else
        icon="󰕾"
    fi
fi

# ASCII bar optimizado
filled=$((display_vol / 10))
case $filled in
    0) bar="░░░░░░░░░░" ;;
    1) bar="█░░░░░░░░░" ;;
    2) bar="██░░░░░░░░" ;;
    3) bar="███░░░░░░░" ;;
    4) bar="████░░░░░░" ;;
    5) bar="█████░░░░░" ;;
    6) bar="██████░░░░" ;;
    7) bar="███████░░░" ;;
    8) bar="████████░░" ;;
    9) bar="█████████░" ;;
    *) bar="██████████" ;;
esac

# Color logic
if [ "$is_muted" = "true" ] || [ "$vol_int" -lt 10 ]; then
    color="%{F#8b5a9f}"  # muted / dark purple
elif [ "$vol_int" -lt 50 ]; then
    color="%{F#d5a8f0}"  # medium pastel purple
else
    color="%{F#cb6ce6}"  # bright purple
fi

# Output
echo "${color}${icon} [${bar}] ${display_vol}%{F-}"
