#!/bin/bash
playerctl pause
IMG1=`find ~/.dotfiles/wallpapers | shuf -n 1`
# IMG2=`find /home/k1/Downloads/wallpaper -regex '.*1920_.*\(png\)$' | shuf -n 1`
#montage $IMG1 $IMG2 -tile 2x1 -geometry +0+0 /tmp/shot.png
# i3lock -p win -fi $IMG2
swaylock --indicator-caps-lock --show-keyboard-layout --ignore-empty-password --show-failed-attempts --daemonize --color 000000 --image /var/tmp/wallpaper.jpg
