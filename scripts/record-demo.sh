#!/usr/bin/env bash

# Efrit Demo Recording Script
# Records a demo of Efrit capabilities using asciinema

set -e

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DEMO_DIR="${PROJECT_ROOT}/demos"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
CAST_FILE="${DEMO_DIR}/efrit-demo-${TIMESTAMP}.cast"
SESSION_NAME="efrit-demo-${TIMESTAMP}"

# Create demos directory
mkdir -p "$DEMO_DIR"

# Check dependencies
check_dependency() {
    if ! command -v "$1" &> /dev/null; then
        echo "Error: $1 is not installed"
        echo "Please install $1 first"
        exit 1
    fi
}

check_dependency tmux
check_dependency asciinema
check_dependency emacs

# Kill any existing demo sessions
tmux kill-session -t "$SESSION_NAME" 2>/dev/null || true

# Start tmux session
echo "Starting demo session..."
tmux new-session -d -s "$SESSION_NAME" -x 80 -y 30

# Start recording
echo "Starting asciinema recording..."
asciinema rec \
    --title "Efrit: AI-Powered Emacs Assistant Demo" \
    --idle-time-limit 3 \
    --command "tmux attach-session -t $SESSION_NAME" \
    "$CAST_FILE" &

ASCIINEMA_PID=$!
sleep 2

# Run demo commands
echo "Running demo..."
tmux send-keys -t "$SESSION_NAME" "cd $PROJECT_ROOT" Enter
sleep 1
tmux send-keys -t "$SESSION_NAME" "emacs -Q -nw -l efrit.el -l efrit-demo.el" Enter
sleep 3

# Execute demo examples
tmux send-keys -t "$SESSION_NAME" "M-x efrit-demo-interactive" Enter
sleep 2

# Let the demo run
echo "Demo running... Press Ctrl-C to stop"
wait $ASCIINEMA_PID

# Cleanup
tmux kill-session -t "$SESSION_NAME" 2>/dev/null || true

echo "Demo recording saved to: $CAST_FILE"
echo ""
echo "To play the recording:"
echo "  asciinema play $CAST_FILE"
echo ""
echo "To convert to GIF (requires agg):"
echo "  agg $CAST_FILE ${CAST_FILE%.cast}.gif"