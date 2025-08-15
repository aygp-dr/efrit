#!/bin/bash
# Interactive test script for efrit-remote-queue

set -e

QUEUE_DIR="$HOME/.emacs.d/efrit-queue"
EXPERIMENTS_DIR="$(dirname "$0")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}=== Efrit Remote Queue Test ===${NC}"
echo "Queue directory: $QUEUE_DIR"
echo ""

# Function to ensure queue directories exist
setup_queue_dirs() {
    echo -e "${YELLOW}Setting up queue directories...${NC}"
    mkdir -p "$QUEUE_DIR"/{requests,responses,processing,archive}
    echo -e "${GREEN}✓ Queue directories ready${NC}"
    echo ""
}

# Function to send a test request
send_request() {
    local id="$1"
    local action="$2"
    local content="$3"
    
    local request_file="$QUEUE_DIR/requests/${id}.json"
    
    cat > "$request_file" << EOF
{
  "id": "${id}",
  "action": "${action}",
  "content": ${content},
  "timeout": 30
}
EOF
    
    echo -e "${GREEN}✓ Sent request ${id}${NC}"
    echo "  Action: $action"
    echo "  File: $request_file"
}

# Function to wait for response
wait_for_response() {
    local id="$1"
    local response_file="$QUEUE_DIR/responses/${id}.json"
    local timeout=10
    local elapsed=0
    
    echo -n "Waiting for response..."
    while [ ! -f "$response_file" ] && [ $elapsed -lt $timeout ]; do
        sleep 0.5
        echo -n "."
        elapsed=$((elapsed + 1))
    done
    echo ""
    
    if [ -f "$response_file" ]; then
        echo -e "${GREEN}✓ Response received!${NC}"
        echo "Content:"
        jq . "$response_file" 2>/dev/null || cat "$response_file"
        return 0
    else
        echo -e "${RED}✗ Timeout waiting for response${NC}"
        return 1
    fi
}

# Function to monitor queue activity
monitor_queue() {
    echo -e "${YELLOW}Monitoring queue directories...${NC}"
    echo "Press Ctrl-C to stop"
    echo ""
    
    watch -n 1 "
        echo '=== Queue Status ==='
        echo ''
        echo 'Requests pending:'
        ls -la $QUEUE_DIR/requests/ 2>/dev/null | tail -n +4 || echo '  (empty)'
        echo ''
        echo 'Processing:'
        ls -la $QUEUE_DIR/processing/ 2>/dev/null | tail -n +4 || echo '  (empty)'
        echo ''
        echo 'Responses ready:'
        ls -la $QUEUE_DIR/responses/ 2>/dev/null | tail -n +4 || echo '  (empty)'
        echo ''
        echo 'Recent archives:'
        ls -lt $QUEUE_DIR/archive/ 2>/dev/null | head -5 || echo '  (empty)'
    "
}

# Function to clean queue
clean_queue() {
    echo -e "${YELLOW}Cleaning queue directories...${NC}"
    rm -f "$QUEUE_DIR"/requests/*.json 2>/dev/null || true
    rm -f "$QUEUE_DIR"/responses/*.json 2>/dev/null || true
    rm -f "$QUEUE_DIR"/processing/*.json 2>/dev/null || true
    echo -e "${GREEN}✓ Queue cleaned${NC}"
}

# Function to run test sequence
run_test_sequence() {
    echo -e "${BLUE}=== Running Test Sequence ===${NC}"
    echo ""
    
    # Test 1: Simple evaluation
    echo "Test 1: Simple math evaluation"
    send_request "test-math-$(date +%s)" "eval" '"(+ 2 2)"'
    wait_for_response "test-math-$(date +%s)"
    echo ""
    
    # Test 2: Buffer information
    echo "Test 2: Get buffer list"
    send_request "test-buffers-$(date +%s)" "eval" '"(mapcar #'\''buffer-name (buffer-list))"'
    wait_for_response "test-buffers-$(date +%s)"
    echo ""
    
    # Test 3: System information
    echo "Test 3: Emacs version"
    send_request "test-version-$(date +%s)" "eval" '"emacs-version"'
    wait_for_response "test-version-$(date +%s)"
    echo ""
    
    # Test 4: Error handling
    echo "Test 4: Error handling"
    send_request "test-error-$(date +%s)" "eval" '"(this-function-does-not-exist)"'
    wait_for_response "test-error-$(date +%s)"
    echo ""
}

# Function to start queue in Emacs
start_queue_daemon() {
    echo -e "${YELLOW}Starting Efrit queue daemon...${NC}"
    
    local elisp_code='
(add-to-list '\''load-path "'"$(cd "$EXPERIMENTS_DIR/../../lisp" && pwd)"'")
(require '\''efrit-remote-queue)
(efrit-remote-queue-start)
(message "Queue daemon started at %s" efrit-remote-queue-directory)
(while t (sit-for 1))
'
    
    emacs --batch --eval "$elisp_code" &
    local emacs_pid=$!
    
    echo -e "${GREEN}✓ Queue daemon started (PID: $emacs_pid)${NC}"
    echo "Press Enter to stop the daemon..."
    read
    
    kill $emacs_pid 2>/dev/null || true
    echo -e "${YELLOW}Queue daemon stopped${NC}"
}

# Main menu
show_menu() {
    echo ""
    echo "Choose an option:"
    echo "1) Setup queue directories"
    echo "2) Send test request"
    echo "3) Run test sequence"
    echo "4) Monitor queue (live)"
    echo "5) Start queue daemon"
    echo "6) Clean queue"
    echo "7) Exit"
    echo ""
    read -p "Option: " choice
    
    case $choice in
        1) setup_queue_dirs ;;
        2) 
            read -p "Request ID: " id
            read -p "Action (eval/chat/efrit-do): " action
            read -p "Content (JSON string): " content
            send_request "$id" "$action" "$content"
            wait_for_response "$id"
            ;;
        3) run_test_sequence ;;
        4) monitor_queue ;;
        5) start_queue_daemon ;;
        6) clean_queue ;;
        7) exit 0 ;;
        *) echo -e "${RED}Invalid option${NC}" ;;
    esac
}

# Main loop
setup_queue_dirs
while true; do
    show_menu
done