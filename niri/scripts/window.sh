#!/usr/bin/env bash

# obtener la ventana activa usando niri
window_info=$(niri msg focused-window 2>/dev/null)

# extraer app id y title usando grep y sed
app_id=$(echo "$window_info" | grep 'App ID:' | sed 's/.*App ID: "\(.*\)"/\1/')
title=$(echo "$window_info" | grep 'Title:' | sed 's/.*Title: "\(.*\)"/\1/')

# limitar el tamaño del título a 20 caracteres
max_len=20
if [ ${#title} -gt $max_len ]; then
    title="${title:0:max_len}..."
fi

# mostrar "no window" si no hay info
if [[ -z "$app_id" && -z "$title" ]]; then
    echo "no window"
else
    echo "$app_id | $title"
fi

