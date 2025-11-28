#!/bin/sh

# Pantalla interna siempre on
xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal

# Si HDMI-1-0 está conectado → lo enciendo a la derecha
if xrandr | grep -q "HDMI-1-0 connected"; then
    xrandr --output HDMI-1-0 --mode 1920x1080 --right-of eDP-1 --rotate normal
else
    # si no, lo apago para que XMonad no lo cuente como pantalla
    xrandr --output HDMI-1-0 --off
fi
