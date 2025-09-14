#!/usr/bin/env bash
# emacsclient-wrapper.sh
# Wrapper for emacsclient on Wayland/X11 that supports emacsclient flags.

start_emacs_daemon() {
    if emacsclient -e t >/dev/null 2>&1; then
        echo "daemon is running"
    else
        /usr/bin/emacs --daemon
        echo "started daemon"
    fi
}

use_emacsclient() {
    local opts=()
    local files=()
    local seen_delim=0

    # --- Split args: options first, then files ---
    for arg in "$@"; do
        if [[ $seen_delim -eq 0 ]]; then
            if [[ $arg == "--" ]]; then
                seen_delim=1
                continue
            elif [[ $arg == -* ]]; then
                opts+=("$arg")
                continue
            fi
            seen_delim=1
        fi
        files+=("$arg")
    done

    local frames
    frames=$(emacsclient -e "(length (frame-list))" 2>/dev/null)

    if [[ "$frames" -gt 1 ]]; then
        for file in "${files[@]}"; do
            # Escape single quotes for Elisp string
            safe=$(printf "%s" "$file" | sed "s/'/\\\\'/g")
            emacsclient "${opts[@]}" -e "(nrv/open-or-create-file-buffer '$safe')"
        done
    else
        # No frame yet
        emacsclient -cn "${opts[@]}" -- "${files[@]}"
    fi
}

start_emacs_daemon
use_emacsclient "$@"
