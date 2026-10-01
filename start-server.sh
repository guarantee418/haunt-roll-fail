#!/usr/bin/env bash
# Start a local HRF (hrf.im) server on http://localhost:7070
# Usage: ./start-server.sh        (Ctrl+C to stop)
set -euo pipefail

HRF_DIR="$(cd "$(dirname "$0")" && pwd)"
PORT=7070
URL="http://localhost:$PORT"
ARGS="../good-game-database ../haunt-roll-fail $URL $URL/hrf/ $PORT"

if [ ! -f "$HRF_DIR/haunt-roll-fail/target/scala-2.13/hrf-fastopt.js" ]; then
    echo "Client not built. Run: cd \"$HRF_DIR/haunt-roll-fail\" && sbt fastOptJS" >&2
    exit 1
fi

cd "$HRF_DIR/good-game"

# Create the database only on first start
if [ ! -f "$HRF_DIR/good-game-database.properties" ]; then
    echo "Creating database..."
    sbt "run create $ARGS"
fi

echo "Starting server at $URL/play  (Ctrl+C to stop)"
exec sbt "run run $ARGS"
