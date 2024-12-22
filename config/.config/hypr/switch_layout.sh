#!/usr/bin/bash
layout=$(hyprctl getoption general:layout | awk -F ': ' '/str/ {print $2}')
if [[ "$layout" == "master" ]]; then
	hyprctl keyword general:layout dwindle
	
elif [[ "$layout" == "dwindle" ]]; then
	hyprctl keyword general:layout master

fi
