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
	# Count existing frames
	frames=$(emacsclient -e "(length (frame-list))" 2>/dev/null)
	if [[ "$frames" -lt 2 ]]; then # for some reason starts counting at 2
		emacsclient -c
	fi
	for file in "$@"; do
		emacsclient -e "(nrv/open-or-create-file-buffer \"$file\")"
	done
}

# Start daemon if needed
start_emacs_daemon

use_emacsclient $@
