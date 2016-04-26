# coding: utf-8

import os

from i3pystatus import Status

status = Status(standalone=True)

status.register("clock", format='%A %B|%m %-d %X')
status.register("load", format='{avg1} {avg5}')
status.register("cpu_usage", format='C: {usage:02}%')
status.register("mem", format='M: {percent_used_mem:.0f}%', warn_percentage=70, color='#ffffff')

status.register("network", format_up='{interface} ↑{bytes_sent:>3}K ↓{bytes_recv:>3}K', start_color="#ffffff")
status.register(
    "online",
    format_online='\uF00C', color='#55ff55',
    format_offline='\uF00D', color_offline='#ff5555'
)
if os.listdir('/sys/class/power_supply'):
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

status.register(
    "keyboard_locks",
    format='{num} {scroll}',
    caps_on='C', caps_off='_',
    num_on='N', num_off='_',
    scroll_on='S', scroll_off='_',
)

status.register("uptime", format='up {days}d')
status.register(
    "weather",
    format="{current_temp} - ↓{min_temp} ↑{max_temp}",
    location_code="IRXX0018:1:IR",
    colorize=True
)

status.register("alsa", interval=0.5)
status.register("now_playing", format='{status} {artist} - {title} {song_length}')

status.run()
