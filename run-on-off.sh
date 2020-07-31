#!/bin/bash
echo "Starting on-off..."
cd "$(dirname "$0")" # go into directory of script
sleep 2
java -jar Infiltration.jar on-off --log --own-socket 192.168.0.24:51720
