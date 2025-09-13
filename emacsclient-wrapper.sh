#!/usr/bin/env bash
# emacsclient-wrapper.sh

start_emacs_daemon() {
    if ! pgrep -x emacs >/dev/null; then
        /usr/bin/emacs --daemon
        # wait until socket is ready
        for i in {1..20}; do
            emacsclient -e t >/dev/null 2>&1 && break
            sleep 0.1
        done
    fi
}

use_emacsclient() {
    frames=$(emacsclient -e "(length (frame-list))" 2>/dev/null | tr -d '"[:space:]')
    if [[ -n "$frames" && "$frames" -gt 0 ]]; then
        if [[ $# -gt 0 ]]; then
            emacsclient -n -c "$@"
        else
            emacsclient -n -c
        fi
    else
        # No frame yet
        if [[ $# -gt 0 ]]; then
            emacsclient -n -c "$@"
        else
            emacsclient -n -c
        fi
    fi
}

start_emacs_daemon
use_emacsclient "$@"
