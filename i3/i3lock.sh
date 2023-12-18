#!/bin/bash
playerctl pause
IMG1=`find /home/k1/Downloads/wallpaper -regex '.*1366_.*\(png\)$' | shuf -n 1`
# IMG2=`find /home/k1/Downloads/wallpaper -regex '.*1920_.*\(png\)$' | shuf -n 1`
#montage $IMG1 $IMG2 -tile 2x1 -geometry +0+0 /tmp/shot.png
# i3lock -p win -fi $IMG2
swaylock --ignore-empty-password --show-failed-attempts --daemonize --color 000000 --image $IMG1 --show-keyboard-layout --indicator-caps-lock