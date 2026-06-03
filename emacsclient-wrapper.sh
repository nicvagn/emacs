#!/usr/bin/env bash
# emacsclient-wrapper.sh
# Wrapper for emacsclient that only spawns two windows

start_emacs_daemon() {
    if [[ -S /run/user/$(id -u)/emacs/server ]]
    then
        echo "daemon is up"
	else
		nohup /usr/bin/emacs --daemon & &>/dev/null
		echo "started daemon"
	fi
}

use_emacsclient() {
    # check for -nw
    if [[ $1 == "-nw" ]]
    then
        emacsclient "$@"
        return
    else  # GUI Emacs
        for file in $@
        do
            echo "(nrv/open-or-create-file-buffer \"$file\") - sent to Emacs"
            emacsclient -e "(nrv/open-or-create-file-buffer \"$file\")"
        done
    fi

	# Count existing frames
	frames=$(emacsclient -e "(length (frame-list))")
	if [[ "$frames" -lt 3 ]]
    then
        emacsclient -c &
    else
        emacsclient -e "(select-frame-set-input-focus (selected-frame))"
        echo "Reused frame. frames >= 2."
    fi

}

# Start daemon if needed
start_emacs_daemon

# Wait until the daemon is ready
for i in $(seq 1 10)
do
    emacsclient -e "t" &>/dev/null && break
    sleep 1
done

use_emacsclient "$@"
