# coding: utf-8

import os

from i3pystatus import Status
from i3pystatus.calendar.google import Google
from i3pystatus.weather import weathercom

status = Status(standalone=True)

status.register("clock", format=('AMS %H:%M', 'Europe/Amsterdam'))
status.register("clock", format=('%a %b %m-%d THR %H:%M',  'Asia/Tehran'))
#status.register("load", format='{avg1} {avg5}')
status.register("cpu_usage", format='C: {usage:02}%')
status.register("temp",
                dynamic_color=True)
status.register("mem", format='M: {percent_used_mem:.0f}%', warn_percentage=70, color='#ffffff')

status.register("network", format_up='Net:{bytes_recv:>3}KB/s', detect_active=True, start_color="#ffffff")
#status.register(
#    "online",
#    format_online='\uF00C', color='#55ff55',
#    format_offline='\uF00D', color_offline='#ff5555'
#)
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

#status.register(
#    "keyboard_locks",
#    format='{num} {scroll}',
#    caps_on='C', caps_off='_',
#    num_on='N', num_off='_',
#    scroll_on='S', scroll_off='_',
#)

#status.register("uptime", format='up {days}d')
#status.register(
#    "weather",
#    backend=weathercom.Weathercom(
#        location_code="NLXX0018:1:NL",
#        update_error='<span color="#ff0000">!</span>'
#    ),
#    format="{current_temp} - ↓{min_temp} ↑{max_temp}",
#    colorize=True,
#    hints={'markup': 'pango'}
#)

status.register("alsa")
status.register("pulseaudio")
#status.register("backlight", format='💡{percentage}%')
#status.register("pomodoro", sound='/usr/share/sounds/sound-icons/beginning-of-line')
#status.register("timewarrior", format='{tags} {duration}')
#status.register("now_playing", player='spotify', format='{status} {artist} - {title} {song_length}')
status.register("now_playing", format='{status} {artist} - {title} {song_length}')

status.run()
