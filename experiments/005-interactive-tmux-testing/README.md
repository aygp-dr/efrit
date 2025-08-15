# Experiment 005: Interactive Tmux Testing with Asciinema Recording

## Overview
This experiment documents how to test Efrit interactively using tmux sessions, capture the testing process with asciinema, and convert recordings to GIF files for documentation.

## Testing Process

### 1. Initial Setup
```bash
# Create tmux session
tmux new-session -d -s efrit-test -c /home/jwalsh/ghq/github.com/aygp-dr/efrit

# Start Emacs without init file
tmux send-keys -t efrit-test "emacs -Q" C-m

# Wait for Emacs to start
sleep 2

# Capture screen to verify
tmux capture-pane -t efrit-test -p
```

### 2. Loading Efrit
```bash
# Use M-x load-file
tmux send-keys -t efrit-test "M-x"
tmux send-keys -t efrit-test "load-file" C-m
tmux send-keys -t efrit-test "lisp/efrit.el" C-m

# Verify load success
tmux capture-pane -t efrit-test -p
```

### 3. Testing efrit-do
```bash
# Run efrit-do
tmux send-keys -t efrit-test "M-x"
tmux send-keys -t efrit-test "efrit-do" C-m

# Send command
tmux send-keys -t efrit-test "show me the current time" C-m

# Check result
sleep 3
tmux capture-pane -t efrit-test -p
```

### 4. Testing Complex Commands (Haiku Example)
```bash
# Run efrit-do again
tmux send-keys -t efrit-test "M-x"
tmux send-keys -t efrit-test "efrit-do" C-m

# Send haiku command
tmux send-keys -t efrit-test "write and show four haikus about IDEs in different buffers" C-m

# Wait for API and execution
sleep 8
tmux capture-pane -t efrit-test -p
```

## Key Learnings

### Reading tmux Panes
- Use `tmux capture-pane -t session-name -p` to capture full screen
- Don't use `head` or `tail` when debugging - need full context
- The `-p` flag prints to stdout

### Sending Keys to tmux
- Control sequences: `tmux send-keys -t session "C-x" "b"`
- Regular text: `tmux send-keys -t session "text" C-m`
- Special keys: `Tab`, `C-g`, `C-m` (Enter)

### Emacs in tmux Considerations
- Use `emacs -Q` to avoid init file complications
- Wait after commands with `sleep` for async operations
- Check `*Messages*` buffer for debugging

## Demo Recording Setup

### Prerequisites
```bash
# Install asciinema (FreeBSD)
pkg install asciinema

# Install agg for GIF conversion
pkg install agg
# or build from source:
# git clone https://github.com/asciinema/agg
# cd agg && cargo build --release
```

### Recording Script
```bash
#!/usr/bin/env bash
# record-efrit-demo.sh

# Create demos directory
mkdir -p demos

# Start recording
asciinema rec demos/efrit-haiku-demo.cast \
  --title "Efrit: Creating Haikus in Multiple Buffers" \
  --idle-time-limit 2 \
  --command "bash run-demo.sh"

# Convert to GIF
agg demos/efrit-haiku-demo.cast demos/efrit-haiku-demo.gif \
  --font-size 14 \
  --theme monokai \
  --speed 1.5

echo "Demo saved to demos/efrit-haiku-demo.gif"
```

## Automated Demo Script

```bash
#!/usr/bin/env bash
# run-demo.sh - Automated demo for recording

set -e

echo "=== Efrit Interactive Demo ==="
echo ""
echo "Starting tmux session with Emacs..."
sleep 2

# Create and setup tmux session
tmux new-session -d -s demo -c "$(pwd)"
tmux send-keys -t demo "emacs -Q" C-m
sleep 3

echo "Loading Efrit..."
tmux send-keys -t demo "M-x"
sleep 0.5
tmux send-keys -t demo "load-file" C-m
sleep 0.5
tmux send-keys -t demo "lisp/efrit.el" C-m
sleep 2

echo ""
echo "Testing efrit-do with simple command..."
tmux send-keys -t demo "M-x"
sleep 0.5
tmux send-keys -t demo "efrit-do" C-m
sleep 1
tmux send-keys -t demo "show me the current time" C-m
sleep 4

echo ""
echo "Switching to result buffer..."
tmux send-keys -t demo "C-x" "b"
sleep 0.5
tmux send-keys -t demo "*efrit-do*" C-m
sleep 2

echo ""
echo "Now for something more complex - creating haikus!"
tmux send-keys -t demo "M-x"
sleep 0.5
tmux send-keys -t demo "efrit-do" C-m
sleep 1
tmux send-keys -t demo "write and show four haikus about IDEs in different buffers" C-m

echo ""
echo "Waiting for API response and buffer creation..."
sleep 10

echo ""
echo "Demo complete! Viewing the haikus in split windows."
tmux attach -t demo
```

## Manual Demo Steps (for Asciinema Recording)

When recording manually with asciinema:

1. **Start Recording**
   ```bash
   asciinema rec demos/efrit-manual-demo.cast --title "Efrit Manual Demo"
   ```

2. **In the Recording Session**
   ```bash
   # Show context
   pwd
   ls -la lisp/efrit.el
   
   # Create tmux session
   tmux new-session -s demo
   
   # In tmux: Start Emacs
   emacs -Q
   
   # Load efrit (M-x load-file)
   # Test efrit-do
   # Show results
   
   # Exit tmux (C-b d)
   # Stop recording (exit or Ctrl-D)
   ```

3. **Convert to GIF**
   ```bash
   agg demos/efrit-manual-demo.cast demos/efrit-manual-demo.gif
   ```

## Advanced Recording Options

### High Quality GIF with Custom Settings
```bash
agg input.cast output.gif \
  --font-family "JetBrains Mono" \
  --font-size 16 \
  --line-height 1.4 \
  --theme dracula \
  --speed 1.2 \
  --cols 120 \
  --rows 40
```

### Multiple Demos
Create different demos for different features:
- `efrit-basic-demo.cast` - Simple commands
- `efrit-haiku-demo.cast` - Complex buffer manipulation
- `efrit-chat-demo.cast` - Interactive chat
- `efrit-queue-demo.cast` - Remote queue functionality

## Tips for Good Demos

1. **Pacing**: Add strategic `sleep` commands for readability
2. **Narration**: Echo explanatory text before commands
3. **Clean State**: Always start with `emacs -Q`
4. **Error Handling**: Include `set -e` in scripts
5. **Screen Size**: Use consistent terminal dimensions
6. **Focus**: One feature per demo, keep it short (< 2 minutes)

## Troubleshooting

### Common Issues

1. **tmux session exists**
   ```bash
   tmux kill-session -t demo 2>/dev/null || true
   ```

2. **Emacs daemon conflicts**
   ```bash
   pkill -f "emacs.*daemon" || true
   ```

3. **Asciinema not found**
   ```bash
   # FreeBSD
   pkg install asciinema
   
   # macOS
   brew install asciinema
   
   # Linux
   pip install asciinema
   ```

4. **agg not found**
   ```bash
   # Install Rust first
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   
   # Install agg
   cargo install --git https://github.com/asciinema/agg
   ```

## Example Output

The successful demo should show:
1. Emacs starting in tmux
2. Efrit loading confirmation
3. Simple command execution (current time)
4. Complex command creating 4 haiku buffers
5. Split window layout with all haikus visible

## Next Steps

1. Create automated CI/CD demos
2. Build demo gallery in docs/
3. Add demos to README
4. Create feature-specific mini-demos
5. Build interactive tutorial mode