# Efrit Experiments

This directory contains experimental implementations and proofs of concept for various Efrit features.

## Experiments

### 001-ollama-elisp-only
Demonstrates how to configure Ollama to return ONLY valid Elisp code that can be directly executed by Emacs, without any wrapper text, markdown, or explanations.

### 002-remote-queue-deep-dive
Deep dive analysis and testing of the efrit-remote-queue implementation for managing asynchronous AI requests.

### 003-continue-framework-agents
Implementation of the CONTINUE framework with minimal agent coordination for complex multi-step tasks.

### 004-ollama-tool-calling-elisp
Implements Ollama tool calling functionality in pure Elisp, enabling structured function calls from LLM responses.

### 005-interactive-tmux-testing
Testing framework for interactive tmux sessions and terminal-based workflows.

### 006-autonomous-agent-ollama
Tests self-aware autonomous agent mode with Ollama, implementing the design from AUTONOMOUS_MODE_DESIGN.md with both curl and Emacs examples.

### 007-queue-tangled-system
Comprehensive queue system implementation using org-mode tangling with Pydantic models, JSON schema generation, and multi-agent coordination. Combines QUEUE_USAGE_EXAMPLES.md content with executable literate programming.

## Running Experiments

Each experiment is self-contained in its numbered directory. To run an experiment:

```bash
cd 00X-experiment-name/
./test-*.sh  # If shell script exists
# or
emacs --batch -l test-*.el  # If Elisp test exists
```

See individual experiment README files for specific instructions.

## Adding New Experiments

1. Create a new numbered directory: `00X-description/`
2. Add a README.md explaining the experiment
3. Include all test scripts and code in that directory
4. Keep experiments self-contained and independent