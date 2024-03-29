#!/bin/bash
# Terminate already running bar instances
    killall -q polybar

# Wait until the processes have been shut down
    while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar bar-right &
polybar bar-left &
sleep 1
polybar bar-right-background &
polybar bar-left-background &
