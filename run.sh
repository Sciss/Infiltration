#!/bin/bash
cd "$(dirname "$0")" # go into directory of script

echo "Infiltration start script"
sleep 2
echo "- Setting ALSA controls"
amixer -q -c2 set 'PCM Capture Source' Line
amixer -q -c2 set Line cap
amixer -q -c2 set Speaker Playback 181
amixer -q -c2 set Line Capture 4097
sleep 2
echo "- Starting QJackCtl"
qjackctl &
sleep 10
echo "- Starting Sensor control"
./sensors/sensors_terminal &
sleep 2
echo "- Starting Infiltration"
./run-inf.sh

