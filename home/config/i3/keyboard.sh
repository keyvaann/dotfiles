#!/bin/sh
setxkbmap -layout us,ir
setxkbmap -option;
setxkbmap -option 'grp:lwin_toggle,caps:hyper,grp_led:scroll'
#setxkbmap -option 'caps:hyper,grp_led:scroll'
xmodmap ~/.Xmodmap
