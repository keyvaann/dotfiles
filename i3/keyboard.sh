#!/bin/sh
setxkbmap -layout us,ir
setxkbmap -option;
setxkbmap -option 'grp:rctrl_rshift_toggle,caps:hyper,grp_led:scroll'
xmodmap ~/.Xmodmap
