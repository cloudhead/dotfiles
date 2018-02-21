[ -z "$DISPLAY" -a "$(fgconsole)" -eq 1 ] && exec startx -- -ardelay 180 -arinterval 40
