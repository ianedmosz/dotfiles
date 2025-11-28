#!/bin/sh

get_vol() {
    pactl get-sink-volume @DEFAULT_SINK@ 2>/dev/null \
        | awk 'NR==1 {print $5}'
}

while :; do
    VOL=$(get_vol)
    TIME=$(date '+%Y-%m-%d %H:%M')

    xsetroot -name "VOL ${VOL} | ${TIME}"

    sleep 1
done

