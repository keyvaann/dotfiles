#!/bin/sh

# Get out of town if something errors
set -e

HDMI_STATUS=$(</sys/class/drm/card1-HDMI-A-2/status )  
export DISPLAY=:0

if [ "connected" == "$HDMI_STATUS" ]; then  
    #i3-msg "workspace 3: term, move container to workspace 8, workspace 1: web" #, move workspace to output left, workspace 1: web"
    #sleep 3
    echo $HDMI_STATUS >> /tmp/hdmi
    /usr/bin/xrandr --output HDMI2 --right-of eDP1 --auto >> /tmp/hdmi
else  
    #i3-msg "workspace 8, move container to workspace 3: term, workspace 3: term" #, move workspace to output right, workspace 1: web"
    #sleep 2
    echo $HDMI_STATUS >> /tmp/hdmi
    /usr/bin/xrandr --auto >> /tmp/hdmi
fi
