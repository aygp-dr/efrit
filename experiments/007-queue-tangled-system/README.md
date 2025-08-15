# Experiment 007: Queue System with Org-Mode Tangling

## Overview
This experiment combines the QUEUE_USAGE_EXAMPLES.md content with org-mode tangling support to create a comprehensive, executable documentation system for the Efrit queue system. It includes Python files with Pydantic models for type safety and schema validation.

## Key Features
- **Literate Programming**: Org-mode files with tangled code blocks
- **Type Safety**: Pydantic models for queue message validation
- **Schema Generation**: JSON schemas from Pydantic models
- **Test Harness**: Single-line JSON test generation
- **Agent Coordination**: Multi-agent workflow examples

## Files Generated
- `queue-system.org` - Main org-mode file with tangled code
- `models.py` - Pydantic models for queue messages
- `test_harness.py` - Test generation and validation
- `agent_coordinator.py` - Multi-agent workflow management
- `queue_client.py` - Python client for queue interaction

## Quick Start

### 1. Tangle the org file
```bash
emacs --batch queue-system.org -f org-babel-tangle
```

### 2. Install Python dependencies
```bash
pip install pydantic typing-extensions
```

### 3. Run tests
```bash
python test_harness.py
```

### 4. Start agent coordination
```bash
python agent_coordinator.py
```

## Org-Mode Integration
The org file includes:
- Code blocks that tangle to actual Python files
- JSON schema generation
- Interactive examples
- Agent workflow definitions
- Test case generation

## Agent Types Supported
- L5 Engineer (Implementation)
- L7 Architect (Design)
- Security Expert
- Performance Engineer
- QA Engineer

## Schema Validation
All queue messages are validated against Pydantic schemas, ensuring type safety and structure consistency across the entire system.