ibus-daemon -drx
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --output HDMI-0 --mode 1920x1080 --rate 144
slstatus &
exec dwm
