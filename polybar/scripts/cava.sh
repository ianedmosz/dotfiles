#!/usr/bin/env bash
set -euo pipefail

export LC_ALL=C.UTF-8

bar="▁▂▃▄▅▆▇█"
dict="s/;//g;"

# build the sed map 0→▁, 1→▂, ...
for ((i=0; i<${#bar}; i++)); do
  dict="${dict}s/$i/${bar:$i:1}/g;"
done

# temp cava config (heredoc avoids quote issues)
config_file="$(mktemp -t polybar_cava_XXXX)"
trap 'rm -f "$config_file"' EXIT

cat > "$config_file" <<'CFG'
[general]
bars = 18

[output]
method = raw
raw_target = /dev/stdout
data_format = ascii
ascii_max_range = 10
CFG

# stream to polybar
cava -p "$config_file" | while read -r line; do
  echo "$line" | sed "$dict"
done
