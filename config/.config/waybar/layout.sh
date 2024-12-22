#!/bin/bash
# Script to display the current keyboard layout using hyprctl

# Get the active keymap for the main keyboard
layout=$(hyprctl devices -j | jq -r '.keyboards[] | select(.main == true) | .active_keymap')

# Handle cases where the layout may be empty
if [ -z "$layout" ]; then
    layout="N/A"  # Set a default if layout is not detected
fi

# Output JSON for Waybar
echo -e "{\"text\": \"$layout\", \"tooltip\": \"Current layout: $layout\"}"

