#!/bin/bash

# emacsclient-wrapper.sh
# A robust wrapper for emacsclient that can be used as $EDITOR
# Handles cases where emacs daemon isn't running and provides fallbacks

# Configuration
FALLBACK_EDITOR="${VISUAL:-${EDITOR:-nano}}"
EMACS_SOCKET_NAME="${EMACS_SOCKET_NAME:-server}"
TIMEOUT=5

# Function to check if emacs daemon is running
is_daemon_running() {
    emacsclient -s "$EMACS_SOCKET_NAME" -e "(+ 1 1)" >/dev/null 2>&1
}

# Function to start emacs daemon
start_daemon() {
    echo "Starting emacs daemon..." >&2
    /usr/bin/emacs --daemon="$EMACS_SOCKET_NAME" >/dev/null 2>&1
    
    # Wait for daemon to start
    local count=0
    while [ $count -lt $TIMEOUT ]; do
        if is_daemon_running; then
            echo "Emacs daemon started successfully." >&2
            return 0
        fi
        sleep 1
        count=$((count + 1))
    done
    
    echo "Failed to start emacs daemon within ${TIMEOUT} seconds." >&2
    return 1
}

# Function to use emacsclient
use_emacsclient() {
    local args=()
    
    # Check if we're in a terminal or GUI environment
    if [ -n "$DISPLAY" ] || [ -n "$WAYLAND_DISPLAY" ]; then
        # GUI environment - create new frame
        args+=("-c")
    else
        # Terminal environment - use current terminal
        args+=("-t")
    fi
    
    # Add socket name
    args+=("-s" "$EMACS_SOCKET_NAME")
    
    # Add all passed arguments
    args+=("$@")
    
    exec emacsclient "${args[@]}"
}

# Main logic
main() {
    # If no arguments provided, just start emacsclient
    if [ $# -eq 0 ]; then
        if is_daemon_running; then
            use_emacsclient
        elif start_daemon; then
            use_emacsclient
        else
            echo "Falling back to $FALLBACK_EDITOR" >&2
            exec "$FALLBACK_EDITOR"
        fi
        return
    fi
    
    # Handle special cases for certain programs
    case "$1" in
        # Git commit messages, merge conflicts, etc.
        .git/COMMIT_EDITMSG|.git/MERGE_MSG|.git/rebase-merge/*)
            # For git operations, always wait for the editor to close
            if is_daemon_running || start_daemon; then
                emacsclient -s "$EMACS_SOCKET_NAME" -t "$@"
            else
                exec "$FALLBACK_EDITOR" "$@"
            fi
            ;;
        # Systemd service files
        /etc/systemd/system/*|/run/systemd/system/*|/usr/lib/systemd/system/*)
            # For systemd, we need to wait for the editor to close
            if is_daemon_running || start_daemon; then
                emacsclient -s "$EMACS_SOCKET_NAME" -t "$@"
            else
                exec "$FALLBACK_EDITOR" "$@"
            fi
            ;;
        # Default case
        *)
            if is_daemon_running || start_daemon; then
                use_emacsclient "$@"
            else
                exec "$FALLBACK_EDITOR" "$@"
            fi
            ;;
    esac
}

# Handle script being sourced vs executed
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi
