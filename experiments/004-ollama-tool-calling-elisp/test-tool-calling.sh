#!/usr/bin/env bash

# Test script for Ollama tool calling with Elisp primitives

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

echo -e "${BLUE}=== Ollama Tool Calling Test ===${NC}"
echo "Testing Elisp primitive tools with Ollama"
echo ""

# Check if Ollama is running
echo -e "${YELLOW}Checking Ollama connection...${NC}"
if curl -s "$OLLAMA_HOST/api/tags" > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Ollama is running${NC}"
else
    echo -e "${RED}✗ Cannot connect to Ollama at $OLLAMA_HOST${NC}"
    echo "Please start Ollama: ollama serve"
    exit 1
fi

# Check for tool-supporting models
echo -e "${YELLOW}Checking for tool-supporting models...${NC}"
MODELS=$(curl -s "$OLLAMA_HOST/api/tags" | jq -r '.models[].name' | grep -E 'llama3\.1|mistral-nemo|firefunction|command-r' || true)

if [ -z "$MODELS" ]; then
    echo -e "${RED}No tool-supporting models found${NC}"
    echo "Please pull a model that supports tools:"
    echo "  ollama pull llama3.1"
    echo "  ollama pull mistral-nemo"
    exit 1
fi

echo -e "${GREEN}Available tool-supporting models:${NC}"
echo "$MODELS" | sed 's/^/  - /'
MODEL=$(echo "$MODELS" | head -1)
echo -e "${GREEN}Using model: $MODEL${NC}"
echo ""

# Test 1: Simple tool call
echo -e "${BLUE}Test 1: List buffers (single tool)${NC}"
curl -s -X POST "$OLLAMA_HOST/api/chat" \
    -H "Content-Type: application/json" \
    -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"List all open buffers\"}],
        \"tools\": [{
            \"type\": \"function\",
            \"function\": {
                \"name\": \"list-buffers\",
                \"description\": \"List all currently open buffers in Emacs\",
                \"parameters\": {
                    \"type\": \"object\",
                    \"properties\": {
                        \"files-only\": {
                            \"type\": \"boolean\",
                            \"description\": \"If true, list only buffers visiting files\",
                            \"default\": false
                        }
                    },
                    \"required\": []
                }
            }
        }],
        \"stream\": false
    }" | jq '.message.tool_calls' || echo "No tool calls in response"

echo ""

# Test 2: File operation
echo -e "${BLUE}Test 2: Find file (with required parameter)${NC}"
curl -s -X POST "$OLLAMA_HOST/api/chat" \
    -H "Content-Type: application/json" \
    -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Open the init.el file in my emacs config\"}],
        \"tools\": [{
            \"type\": \"function\",
            \"function\": {
                \"name\": \"find-file\",
                \"description\": \"Open a file in Emacs for viewing or editing\",
                \"parameters\": {
                    \"type\": \"object\",
                    \"properties\": {
                        \"filename\": {
                            \"type\": \"string\",
                            \"description\": \"Path to the file to open\"
                        }
                    },
                    \"required\": [\"filename\"]
                }
            }
        }],
        \"stream\": false
    }" | jq '.message.tool_calls' || echo "No tool calls in response"

echo ""

# Test 3: Search operation
echo -e "${BLUE}Test 3: Grep search (multiple parameters)${NC}"
curl -s -X POST "$OLLAMA_HOST/api/chat" \
    -H "Content-Type: application/json" \
    -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Search for 'defun' in all elisp files in the current directory\"}],
        \"tools\": [{
            \"type\": \"function\",
            \"function\": {
                \"name\": \"grep-find\",
                \"description\": \"Search for a pattern in files using grep\",
                \"parameters\": {
                    \"type\": \"object\",
                    \"properties\": {
                        \"pattern\": {
                            \"type\": \"string\",
                            \"description\": \"Search pattern or regex\"
                        },
                        \"directory\": {
                            \"type\": \"string\",
                            \"description\": \"Directory to search in\",
                            \"default\": \".\"
                        },
                        \"file-pattern\": {
                            \"type\": \"string\",
                            \"description\": \"File pattern to search (e.g., *.el)\",
                            \"default\": \"*\"
                        }
                    },
                    \"required\": [\"pattern\"]
                }
            }
        }],
        \"stream\": false
    }" | jq '.' | head -30

echo ""

# Test 4: Multiple tools available
echo -e "${BLUE}Test 4: Multiple tools (let model choose)${NC}"
ALL_TOOLS='[
    {
        "type": "function",
        "function": {
            "name": "dired",
            "description": "Open directory listing in Dired mode",
            "parameters": {
                "type": "object",
                "properties": {
                    "directory": {"type": "string", "description": "Directory path"}
                },
                "required": ["directory"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "find-file",
            "description": "Open a file in Emacs",
            "parameters": {
                "type": "object",
                "properties": {
                    "filename": {"type": "string", "description": "File path"}
                },
                "required": ["filename"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "list-buffers",
            "description": "List all open buffers",
            "parameters": {
                "type": "object",
                "properties": {},
                "required": []
            }
        }
    }
]'

curl -s -X POST "$OLLAMA_HOST/api/chat" \
    -H "Content-Type: application/json" \
    -d "{
        \"model\": \"$MODEL\",
        \"messages\": [{\"role\": \"user\", \"content\": \"Show me what files are in the experiments directory\"}],
        \"tools\": $ALL_TOOLS,
        \"stream\": false
    }" | jq '.message | {content, tool_calls}'

echo ""
echo -e "${GREEN}=== Test Complete ===${NC}"
echo ""
echo "To use in Emacs:"
echo "1. Load ollama-elisp-tools.el"
echo "2. Run: (ollama-elisp-chat-with-tools \"your request\")"
echo "3. View history: M-x ollama-show-tool-history"