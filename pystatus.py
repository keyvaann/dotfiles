# coding: utf-8

from i3pystatus import Status

status = Status(standalone=True)

# Displays clock like this:
# Tue 30 Jul 11:59:46 PM KW31
#                          ^-- calendar week
status.register("clock", format="%-d %b %X",)

# Shows the average load of the last minute and the last 5 minutes
# (the default value for format is used)
status.register("load")

# The battery monitor has many formatting options, see README for details

# This would look like this, when discharging (or charging)
# ↓14.22W 56.15% [77.81%] 2h:41m
# And like this if full:
# =14.22W 100.0% [91.21%]
#
# This would also display a desktop notification (via D-Bus) if the percentage
# goes below 5 percent while discharging. The block will also color RED.
# If you don't have a desktop notification demon yet, take a look at dunst:
#   http://www.knopwob.org/dunst/
status.register(
    "battery",
    format="{status} {percentage:.0f}%",
    alert=True,
    alert_percentage=15,
    status={
        "DIS": "↓",
        "CHR": "↑",
        "FULL": "=",
    },
)

# Shows the address and up/down state of eth0. If it is up the address is shown in
# green (the default value of color_up) and the CIDR-address is shown
# (i.e. 10.10.10.42/24).
# If it's down just the interface name (eth0) will be displayed in red
# (defaults of format_down and color_down)
#
# Note: the network module requires PyPI package netifaces
status.register(
    "network",
    interface="wlan0",
    format_up="{essid} {quality}% {kbs}KB/s",
)


status.register("alsa", interval=0.5)
# status.register("backlight", format='{percentage}')
status.register("cpu_usage", format='C: {usage}%')
status.register("mem", format='M: {percent_used_mem}%')

# Shows mpd status
# Format:
# Cloud connected▶Reroute to Remain

status.run()
