Xephyr -br -ac -reset -screen 800x600 :1 &
sleep 1s
export DISPLAY=:1
xmonad &
