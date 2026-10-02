#!/usr/bin/env bash
# Run the live server (https://games.clean5110.com) in the tmux session "hrf".
# Run it on the server, from the ~/hrf checkout:
#   ./live-server.sh start              start the server unless it is running
#   ./live-server.sh stop               stop the server
#   ./live-server.sh restart            stop, then start
#   ./live-server.sh deploy             check out origin/main, then restart
#   ./live-server.sh log                show the last lines of the server output
#   ./live-server.sh install-autostart  start the server after every reboot
set -uo pipefail

# cron runs @reboot jobs with a minimal environment
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

HRF_DIR="$(cd "$(dirname "$0")" && pwd)"
SESSION=hrf
PORT=7070
URL="https://games.clean5110.com"
ARGS="../good-game-database ../haunt-roll-fail $URL $URL/hrf/ $PORT"

running() {
    tmux has-session -t "$SESSION" 2>/dev/null
}

start() {
    if running; then
        echo "Already running in tmux session $SESSION (use restart to restart it)"
        return
    fi

    # A login shell, so PATH and SBT_OPTS come from the profile and ~/.bashrc
    tmux new-session -d -s "$SESSION" -c "$HRF_DIR/good-game" "bash -l"
    tmux send-keys -t "$SESSION" "sbt \"run run $ARGS\"" Enter

    for i in $(seq 1 60); do
        if tmux capture-pane -pt "$SESSION" | grep -q "Started server"; then
            echo "Server started at $URL/play"
            return
        fi
        sleep 5
    done

    echo "Server has not started after 5 minutes. Output:"
    log
    return 1
}

stop() {
    tmux kill-session -t "$SESSION" 2>/dev/null
    # The brackets keep pkill from matching this script's own command line
    pkill -f "hrf[.]gg[.]GoodGame"
    sleep 3
    echo "Server stopped"
}

log() {
    tmux capture-pane -pt "$SESSION" -S -200 | grep -v '^\s*$' | tail -40
}

deploy() {
    stop
    cd "$HRF_DIR"
    git fetch -q origin main
    # -f: builds on the server modify committed target/ files
    git checkout -q -f -B hrf origin/main
    git log --oneline -1
    # Run the checked-out version of this script, which may have changed
    exec "$HRF_DIR/live-server.sh" start
}

install_autostart() {
    local line="@reboot sleep 30 && $HRF_DIR/live-server.sh start >> $HOME/hrf-autostart.log 2>&1"

    if crontab -l 2>/dev/null | grep -qF "live-server.sh start"; then
        echo "Autostart is already installed:"
    else
        (crontab -l 2>/dev/null; echo "$line") | crontab -
        echo "Installed autostart:"
    fi

    crontab -l | grep -F "live-server.sh start"
}

case "${1:-}" in
    start) start ;;
    stop) stop ;;
    restart) stop; start ;;
    deploy) deploy ;;
    log) log ;;
    install-autostart) install_autostart ;;
    *)
        sed -n '2,9p' "$0" | sed 's/^# \{0,1\}//'
        exit 1
        ;;
esac
