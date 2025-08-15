# Experiment 001: Ollama Elisp-Only Responses

## Overview
This experiment demonstrates how to configure Ollama to return ONLY valid Elisp code that can be directly executed by Emacs, without any wrapper text, markdown, or explanations.

## Key Innovation
Uses Ollama's structured output with JSON schema to enforce that responses contain only executable Elisp code. The system prompt is carefully crafted to prevent the model from adding explanations or formatting.

## Files
- `ollama-elisp-only.el` - Main Elisp module for Ollama integration
- `test-ollama-elisp.el` - Test suite and examples
- `test-with-curl.sh` - Shell script demonstrating raw HTTP API usage
- `README.md` - This documentation

## Quick Start

### 1. Test with curl (no Emacs required)
```bash
./test-with-curl.sh
```

This script will:
- Auto-discover available Ollama models
- Select the best coding model (prefers qwen2.5-coder)
- Show example structured output requests
- Demonstrate the JSON schema approach

### 2. Use in Emacs
```elisp
;; Load the module
(load-file "ollama-elisp-only.el")

;; Test connection
(ollama-elisp-test-connection)

;; Query examples
(ollama-elisp-query "list files in current directory")
;; Returns: ("file1.el" "file2.el" ...)

(ollama-elisp-query "what is 2 + 2")
;; Returns: 4

(ollama-elisp-query "get current time")
;; Returns: "Thu Jan 15 10:30:45 2025"
```

### 3. Interactive Commands
- `M-x ollama-elisp-query` - Query and evaluate response
- `M-x ollama-elisp-query-insert` - Insert generated Elisp at point
- `M-x ollama-elisp-query-region` - Replace region with Elisp
- `M-x ollama-elisp-context-query` - Query with buffer context

## How It Works

### System Prompt
The system prompt enforces strict rules:
1. ENTIRE response must be valid Elisp
2. NO explanations or markdown
3. Response goes directly to `eval`
4. Use native Elisp functions for all operations

### Structured Output
Uses JSON schema to wrap responses:
```json
{
  "type": "object",
  "properties": {
    "elisp": {
      "type": "string",
      "description": "Valid Elisp code only"
    }
  },
  "required": ["elisp"]
}
```

### Example Request/Response

**Request:** "list all .el files"
**Response JSON:**
```json
{
  "elisp": "(directory-files default-directory t \"\\\\.el$\")"
}
```
**Evaluated Result:** List of .el files in current directory

## Configuration

```elisp
;; Change the Ollama host (default: localhost:11434)
(setq ollama-elisp-host "http://localhost:11434")

;; Change the model (auto-detected by curl script)
(setq ollama-elisp-model "qwen2.5-coder:7b")

;; Adjust temperature (0.1 = deterministic, 1.0 = creative)
(setq ollama-elisp-temperature 0.1)
```

## Testing

Run the test suite:
```elisp
(load-file "test-ollama-elisp.el")

;; Run automated tests
(ert-run-tests-interactively "test-ollama-elisp-")

;; Run interactive demo
(ollama-elisp-demo)

;; Benchmark response times
(ollama-elisp-benchmark)
```

## Example Use Cases

1. **File Operations**
   ```elisp
   (ollama-elisp-query "check if init.el exists in .emacs.d")
   ;; => t or nil
   ```

2. **Data Manipulation**
   ```elisp
   (ollama-elisp-query "create an alist of colors to hex codes")
   ;; => '((red . "#FF0000") (green . "#00FF00") (blue . "#0000FF"))
   ```

3. **Buffer Operations**
   ```elisp
   (ollama-elisp-query "count words in current buffer")
   ;; => 1234
   ```

4. **System Information**
   ```elisp
   (ollama-elisp-query "get load average")
   ;; => (0.15 0.20 0.18)
   ```

## Advantages
- **Direct Execution**: No parsing needed, responses are ready to eval
- **No Wrapper Text**: Structured output prevents explanations
- **Type Safety**: JSON schema ensures consistent format
- **Model Agnostic**: Works with any Ollama model (coding models preferred)
- **Error Handling**: Graceful fallbacks if evaluation fails

## Limitations
- Requires Ollama with structured output support
- Best results with coding-focused models
- Complex multi-step operations may need prompt engineering
- Temperature should be kept low for deterministic results

## Future Improvements
- [ ] Add caching for repeated queries
- [ ] Support for async requests
- [ ] Integration with company-mode for completions
- [ ] Multi-turn conversations with context
- [ ] Fine-tuning prompts for specific Elisp idioms