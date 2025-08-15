#!/usr/bin/env sh
# Test Ollama Elisp-only responses using curl commands

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"

echo "${BLUE}=== Ollama Elisp-Only Experiment ===${NC}"
echo "Host: $OLLAMA_HOST"
echo ""

# Step 1: List available models and select best coding model
echo "${YELLOW}Step 1: Discovering available models...${NC}"
echo "Command: curl -s $OLLAMA_HOST/api/tags"
echo ""

MODELS_JSON=$(curl -s "$OLLAMA_HOST/api/tags")
if [ $? -ne 0 ]; then
    echo "${RED}Error: Cannot connect to Ollama at $OLLAMA_HOST${NC}"
    echo "Make sure Ollama is running: ollama serve"
    exit 1
fi

echo "$MODELS_JSON" | jq -r '.models[] | .name' 2>/dev/null || echo "$MODELS_JSON"
echo ""

# Auto-select best coding model (prefer qwen2.5-coder, then any coder model, then fallback)
MODEL=$(echo "$MODELS_JSON" | jq -r '.models[] | .name' | grep -E '(qwen2.5-coder|coder|codellama|deepseek|starcoder)' | head -1)
if [ -z "$MODEL" ]; then
    MODEL=$(echo "$MODELS_JSON" | jq -r '.models[0] | .name')
fi

echo "${GREEN}Selected model: $MODEL${NC}"
echo ""

# Step 2: Show the system prompt
echo "${YELLOW}Step 2: System Prompt for Elisp-Only Responses${NC}"
cat << 'EOF'
You are an Elisp code generator integrated into Emacs.
CRITICAL RULES:
1. Your ENTIRE response must be valid Elisp code that can be executed with `eval`
2. NO explanations, NO comments outside of Elisp comments, NO markdown
3. Use Elisp functions to accomplish tasks
4. Return ONLY executable Elisp code
5. Your response will be directly passed to `eval` - any non-Elisp will cause an error

Example responses:
- For "list files": (directory-files default-directory)
- For "current time": (current-time-string)
- For "add 2+2": (+ 2 2)
EOF
echo ""

# Step 3: Test with structured output
echo "${YELLOW}Step 3: Testing Structured Output with JSON Schema${NC}"
echo "This ensures the response is wrapped in a JSON field containing only Elisp"
echo ""

# Create the JSON schema for structured output
SCHEMA='{
  "type": "object",
  "properties": {
    "elisp": {
      "type": "string",
      "description": "Valid Elisp code only, no markdown or explanations"
    }
  },
  "required": ["elisp"]
}'

# Function to make structured request
make_structured_request() {
    local prompt="$1"
    local request_json=$(cat << EOF
{
  "model": "$MODEL",
  "prompt": "$prompt",
  "system": "You are an Elisp code generator. Return ONLY valid Elisp code that can be executed with eval. No explanations, no markdown, just pure Elisp.",
  "stream": false,
  "format": {
    "type": "json",
    "schema": $SCHEMA
  },
  "temperature": 0.1
}
EOF
)
    
    echo "Request:"
    echo "$request_json" | jq -c .
    echo ""
    echo "Response:"
    curl -s -X POST "$OLLAMA_HOST/api/generate" \
        -H "Content-Type: application/json" \
        -d "$request_json" | jq -r '.response' | jq .
}

# Test cases
echo "${BLUE}--- Test 1: List files in current directory ---${NC}"
make_structured_request "list all files in the current directory"
echo ""

echo "${BLUE}--- Test 2: Mathematical operation ---${NC}"
make_structured_request "calculate the factorial of 5"
echo ""

echo "${BLUE}--- Test 3: String manipulation ---${NC}"
make_structured_request "reverse the string hello world"
echo ""

echo "${BLUE}--- Test 4: Get system information ---${NC}"
make_structured_request "show emacs version and current time"
echo ""

# Step 4: Test without structured output (raw mode)
echo "${YELLOW}Step 4: Testing Raw Mode (without JSON structure)${NC}"
echo "Warning: This mode is less reliable as the model might include explanations"
echo ""

make_raw_request() {
    local prompt="$1"
    local request_json=$(cat << EOF
{
  "model": "$MODEL",
  "prompt": "Return ONLY Elisp code, no explanations: $prompt",
  "system": "You must return ONLY valid Elisp code. No markdown backticks, no explanations, no comments outside Elisp. Just pure executable Elisp.",
  "stream": false,
  "temperature": 0.1
}
EOF
)
    
    echo "Request: $prompt"
    echo "Response:"
    curl -s -X POST "$OLLAMA_HOST/api/generate" \
        -H "Content-Type: application/json" \
        -d "$request_json" | jq -r '.response'
}

echo "${BLUE}--- Raw Test: Simple addition ---${NC}"
make_raw_request "add 10 and 20"
echo ""

# Step 5: Demonstrate model selection strategy
echo "${YELLOW}Step 5: Model Selection Strategy${NC}"
cat << 'EOF'
The script automatically selects the best model for code generation:
1. First choice: qwen2.5-coder (excellent for code)
2. Second choice: Any model with "coder" in the name
3. Third choice: codellama, deepseek-coder, starcoder
4. Fallback: First available model

To override, set OLLAMA_HOST or modify the MODEL variable.
EOF
echo ""

echo "${GREEN}=== Experiment Complete ===${NC}"
echo ""
echo "To use in Emacs:"
echo "1. Load ollama-elisp-only.el"
echo "2. Set ollama-elisp-model to '$MODEL'"
echo "3. Run (ollama-elisp-query \"your prompt\")"
echo ""
echo "The structured output with JSON schema is the most reliable method!"