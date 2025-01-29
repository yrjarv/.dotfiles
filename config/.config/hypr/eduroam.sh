#!/usr/bin/bash

if ! iwctl station wlan0 get-networks | grep -q eduroam; then
    exit 1
fi

sudo systemctl stop NetworkManager
sudo systemctl stop wpa_supplicant
sudo wpa_supplicant -B -i wlan0 -c ~/.config/cat_installer/cat_installer.conf

