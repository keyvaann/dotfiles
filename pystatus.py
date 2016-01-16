# coding: utf-8

import os

from i3pystatus import Status

status = Status(standalone=True)

status.register("clock")
status.register("load", format='{avg1} {avg5} {tasks}')

if os.listdir('/sys/class/power_supply'):
    status.register(
        "keyboard_locks",
        caps_on='C', caps_off='_',
        num_on='N', num_off='_',
        scroll_on='S', scroll_off='_',
    )

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

status.register("network", format_up='{interface}{kbs}KB/s')
status.register("alsa", interval=0.5)
status.register("cpu_usage", format='C: {usage}%')
status.register("mem", format='M: {percent_used_mem}%')
status.register("spotify")

status.run()
