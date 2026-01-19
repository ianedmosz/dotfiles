#!/bin/bash

# Fallback: siempre imprime algo para comprobar
output=""

# Pide workspaces a niri
workspaces=$(niri msg --json workspaces | jq -r '.[] | "\(.idx) \(.active)"')

while read -r idx active; do
    case "$idx" in
        1) icon="一" ;;
        2) icon="二" ;;
        3) icon="三" ;;
        4) icon="四" ;;
        5) icon="五" ;;
        6) icon="六" ;;
        7) icon="七" ;;
        8) icon="八" ;;
        9) icon="九" ;;
        10) icon="十" ;;
        *) icon="$idx" ;;
    esac

    if [ "$active" = "true" ]; then
        output="$output[$icon] "
    else
        output="$output $icon  "
    fi
done <<< "$workspaces"

# Si no salió nada, imprimimos debug para que no quede vacío
if [ -z "$output" ]; then
    echo "⚠ no ws"
else
    echo "$output"
fi


