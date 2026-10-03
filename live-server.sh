#!/usr/bin/env bash
# Run the live server (https://games.clean5110.com) in the tmux session "hrf".
# Run it on the server, from the ~/hrf checkout:
#   ./live-server.sh start              start the server unless it is running
#   ./live-server.sh stop               stop the server
#   ./live-server.sh restart            stop, then start
#   ./live-server.sh deploy             check out origin/main, then restart
#   ./live-server.sh log                show the last lines of the server output
#   ./live-server.sh install-autostart  start the server after every reboot
#   ./live-server.sh auto-deploy        deploy the GitHub branch "deploy" if it moved
#   ./live-server.sh install-auto-deploy  run auto-deploy from cron every 2 minutes
#   ./live-server.sh install-backup     copy the games to the private GitHub repository
#   ./live-server.sh backup-sync        push the games copy, pull fixes (cron, every 5 minutes)
set -uo pipefail

# cron runs @reboot jobs with a minimal environment
export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

HRF_DIR="$(cd "$(dirname "$0")" && pwd)"
SESSION=hrf
PORT=443
URL="https://games.clean5110.com"
ARGS="../good-game-database ../haunt-roll-fail $URL $URL/hrf/ $PORT"
# Last commit of the deploy branch that auto-deploy handled
DEPLOYED="$HOME/hrf-deployed"
# Private repository the server copies the games to (player secrets included),
# its clone, and the deploy key that can write to it
BACKUP_REPO="${HRF_BACKUP_REPO:-git@github.com:guarantee418/hrf-games.git}"
BACKUP_DIR="$HOME/hrf-games"
BACKUP_KEY="$HOME/.ssh/hrf-games"

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

# deploy [commit]: deploy origin/main, or the given commit
deploy() {
    local target="${1:-origin/main}"
    stop
    cd "$HRF_DIR"
    git fetch -q origin main
    # -f: builds on the server modify committed target/ files
    git checkout -q -f -B hrf "$target"
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

stamp() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') $*"
}

# Deploy the branch "deploy" when it points at a commit not handled yet.
# Pushing main to that branch on GitHub deploys it within 2 minutes:
#   git push origin origin/main:refs/heads/deploy
auto_deploy() {
    # One run at a time. -o: the lock is not inherited by the server that
    # deploy starts, so it is released when this run ends.
    if [ -z "${HRF_AUTO_DEPLOY_LOCKED:-}" ]; then
        HRF_AUTO_DEPLOY_LOCKED=1 exec flock -n -E 0 -o "$HOME/.hrf-auto-deploy.lock" "$HRF_DIR/live-server.sh" auto-deploy
    fi

    cd "$HRF_DIR"

    git ls-remote -q --exit-code --heads origin deploy > /dev/null
    case $? in
        0) ;;
        2) return 0 ;; # no deploy branch
        *) stamp "Could not reach GitHub"; return 0 ;;
    esac

    if ! git fetch -q origin main deploy; then
        stamp "git fetch failed"
        return 0
    fi

    local commit
    commit="$(git rev-parse origin/deploy)"

    if [ "$commit" = "$(cat "$DEPLOYED" 2>/dev/null)" ]; then
        return 0
    fi

    # Recorded first, so a commit that fails to start is not retried every 2 minutes
    echo "$commit" > "$DEPLOYED"

    # Only deploy what is already merged into main
    if ! git merge-base --is-ancestor "$commit" origin/main; then
        stamp "Not deploying $(git log --oneline -1 "$commit"): not on main"
        return 0
    fi

    stamp "Deploying $(git log --oneline -1 "$commit")"
    deploy "$commit"
}

install_auto_deploy() {
    local line="*/2 * * * * $HRF_DIR/live-server.sh auto-deploy >> $HOME/hrf-auto-deploy.log 2>&1"

    # Don't redeploy what the deploy branch points at right now
    if [ ! -f "$DEPLOYED" ] && git -C "$HRF_DIR" fetch -q origin deploy 2>/dev/null; then
        git -C "$HRF_DIR" rev-parse origin/deploy > "$DEPLOYED"
    fi

    if crontab -l 2>/dev/null | grep -qF "live-server.sh auto-deploy"; then
        echo "Auto-deploy is already installed:"
    else
        (crontab -l 2>/dev/null; echo "$line") | crontab -
        echo "Installed auto-deploy:"
    fi

    crontab -l | grep -F "live-server.sh auto-deploy"
}

# The server writes every game to $BACKUP_DIR each minute (good-game/Backup.scala)
# and applies the fixes committed to fixes/. This commits that copy, pulls new
# fixes and pushes. The server's files win any conflict.
backup_sync() {
    if [ -z "${HRF_BACKUP_LOCKED:-}" ]; then
        HRF_BACKUP_LOCKED=1 exec flock -n -E 0 "$HOME/.hrf-backup.lock" "$HRF_DIR/live-server.sh" backup-sync
    fi

    [ -d "$BACKUP_DIR/.git" ] || return 0
    cd "$BACKUP_DIR"

    cp "$HRF_DIR/good-game/games-README.md" README.md
    git add -A
    git diff --cached --quiet || git commit -q -m "Games $(date '+%Y-%m-%d %H:%M')"

    git ls-remote -q --exit-code --heads origin main > /dev/null 2>&1
    case $? in
        0)
            if ! git pull -q --rebase -X theirs origin main; then
                # Start from GitHub's version, keep the server's files, restore what only GitHub has
                git rebase --abort 2>/dev/null
                git reset -q origin/main
                git ls-files -z --deleted | xargs -0 -r git checkout --
                git add -A
                git diff --cached --quiet || git commit -q -m "Games $(date '+%Y-%m-%d %H:%M')"
                stamp "Pull did not merge cleanly; committed the server's copy on top of GitHub's"
            fi
            ;;
        2) ;; # empty repository: the push creates main
        *) stamp "Could not reach GitHub"; return 0 ;;
    esac

    git push -q origin HEAD:main || stamp "Push failed"
}

install_backup() {
    mkdir -p "$HOME/.ssh"

    if [ ! -f "$BACKUP_KEY" ]; then
        ssh-keygen -q -t ed25519 -N "" -C "hrf-server games backup" -f "$BACKUP_KEY"
    fi

    local ssh="ssh -i $BACKUP_KEY -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"

    if [ ! -d "$BACKUP_DIR/.git" ] && ! GIT_SSH_COMMAND="$ssh" git clone -q "$BACKUP_REPO" "$BACKUP_DIR"; then
        echo
        echo "Could not clone $BACKUP_REPO. Create it on GitHub as a PRIVATE"
        echo "repository, add this key under Settings > Deploy keys with"
        echo "\"Allow write access\" checked, then run install-backup again:"
        echo
        cat "$BACKUP_KEY.pub"
        return 1
    fi

    git -C "$BACKUP_DIR" config core.sshCommand "$ssh"
    git -C "$BACKUP_DIR" config user.name "HRF server"
    git -C "$BACKUP_DIR" config user.email "hrf-server@users.noreply.github.com"
    # An empty repository: commit to main
    git -C "$BACKUP_DIR" rev-parse -q --verify HEAD > /dev/null || git -C "$BACKUP_DIR" symbolic-ref HEAD refs/heads/main

    # The server checks this every minute, so no restart is needed
    echo "$BACKUP_DIR" > "$HRF_DIR/good-game/backup-dir"

    local line="*/5 * * * * $HRF_DIR/live-server.sh backup-sync >> $HOME/hrf-backup.log 2>&1"

    if crontab -l 2>/dev/null | grep -qF "live-server.sh backup-sync"; then
        echo "Backup sync is already installed:"
    else
        (crontab -l 2>/dev/null; echo "$line") | crontab -
        echo "Installed backup sync:"
    fi

    crontab -l | grep -F "live-server.sh backup-sync"
    echo "The games appear in $BACKUP_REPO within about 6 minutes."
}

case "${1:-}" in
    start) start ;;
    stop) stop ;;
    restart) stop; start ;;
    deploy) deploy "${2:-}" ;;
    log) log ;;
    install-autostart) install_autostart ;;
    auto-deploy) auto_deploy ;;
    install-auto-deploy) install_auto_deploy ;;
    install-backup) install_backup ;;
    backup-sync) backup_sync ;;
    *)
        sed -n '2,13p' "$0" | sed 's/^# \{0,1\}//'
        exit 1
        ;;
esac
