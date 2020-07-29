#!/bin/bash
amixer -c2 set 'PCM Capture Source' Line
amixer -c2 set Line cap
amixer -c2 set Speaker Playback 181
amixer -c2 set Line Capture 4097

