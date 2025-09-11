#!/bin/bash
# emacsclient-wrapper.sh
# Robust wrapper for emacsclient on Wayland/X11

# Function to start daemon if not running
start_emacs_daemon() {
    if ! pgrep -x emacs >/dev/null; then
        /usr/bin/emacs --daemon
        echo "started daemon"
    else 
        echo "daemon is running"
    fi
}

# Function to call emacsclient
use_emacsclient() {
    # Count existing frames (works on X11 and Wayland)
    frames=$(emacsclient -e "(length (frame-list))" 2>/dev/null)

    if [[ "$frames" -gt 0 ]]; then
        # Frames exist
        if [[ $# -gt 0 ]]; then
            emacsclient -n "$@"       # open files in existing frame
            echo "opening file in frame"

        else
            emacsclient -n -c         # create a new empty frame
            echo "creating frame, but others exist"
        fi
    else
        # No frames yet → create a new one
        emacsclient -n -c "$@"
        echo "creating first frame" 
    fi
}

# Start daemon if needed
start_emacs_daemon

# Call wrapper with command-line arguments (files)
use_emacsclient "$@"
