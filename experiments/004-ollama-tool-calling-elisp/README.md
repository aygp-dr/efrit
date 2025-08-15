# Experiment 004: Ollama Tool Calling with Elisp Primitives

## Overview
This experiment implements Ollama's tool calling support using pure Elisp primitives like `dired`, `find-file`, `load-file`, and `grep-find`. These functions become callable tools that the LLM can invoke to interact with Emacs.

## Background
Ollama now supports tool calling (function calling) allowing models to use predefined tools to perform complex tasks. This experiment maps Emacs Lisp functions to the tool calling protocol.

## Tool Definition Format

According to the Ollama blog, tools are defined as:
```json
{
  "type": "function",
  "function": {
    "name": "function_name",
    "description": "What the function does",
    "parameters": {
      "type": "object",
      "properties": {
        "param1": {
          "type": "string",
          "description": "Parameter description"
        }
      },
      "required": ["param1"]
    }
  }
}
```

## Elisp Primitive Tools

### 1. Directory Operations
```elisp
(defvar efrit-tool-dired
  '((type . "function")
    (function . ((name . "dired")
                 (description . "Open directory listing in Dired mode")
                 (parameters . ((type . "object")
                               (properties . ((directory . ((type . "string")
                                                           (description . "Directory path to open")))))
                               (required . ["directory"]))))))

;; Function signature: (dired directory)
```

### 2. File Operations
```elisp
(defvar efrit-tool-find-file
  '((type . "function")
    (function . ((name . "find-file")
                 (description . "Open a file in Emacs")
                 (parameters . ((type . "object")
                               (properties . ((filename . ((type . "string")
                                                          (description . "Path to file")))))
                               (required . ["filename"]))))))

;; Function signature: (find-file filename)
```

### 3. Load Operations
```elisp
(defvar efrit-tool-load-file
  '((type . "function")
    (function . ((name . "load-file")
                 (description . "Load and evaluate an Elisp file")
                 (parameters . ((type . "object")
                               (properties . ((file . ((type . "string")
                                                      (description . "Path to Elisp file")))))
                               (required . ["file"]))))))

;; Function signature: (load-file file)
```

### 4. Search Operations
```elisp
(defvar efrit-tool-grep-find
  '((type . "function")
    (function . ((name . "grep-find")
                 (description . "Search for pattern in files using grep")
                 (parameters . ((type . "object")
                               (properties . ((command . ((type . "string")
                                                         (description . "Grep command to execute")))))
                               (required . ["command"]))))))

;; Function signature: (grep-find command)
```

### 5. Buffer Operations
```elisp
(defvar efrit-tool-list-buffers
  '((type . "function")
    (function . ((name . "list-buffers")
                 (description . "List all open buffers")
                 (parameters . ((type . "object")
                               (properties . ((files-only . ((type . "boolean")
                                                            (description . "List only file buffers")
                                                            (default . :false)))))
                               (required . []))))))

;; Function signature: (list-buffers &optional files-only)
```

### 6. Window Operations
```elisp
(defvar efrit-tool-split-window
  '((type . "function")
    (function . ((name . "split-window")
                 (description . "Split the current window")
                 (parameters . ((type . "object")
                               (properties . ((direction . ((type . "string")
                                                           (enum . ["horizontal" "vertical"])
                                                           (description . "Split direction")))))
                               (required . []))))))

;; Function signature: (split-window-below) or (split-window-right)
```

## Implementation

```elisp
;;; ollama-elisp-tools.el --- Ollama tool calling with Elisp primitives

(require 'json)
(require 'url)

(defvar ollama-elisp-tools-list
  (list efrit-tool-dired
        efrit-tool-find-file
        efrit-tool-load-file
        efrit-tool-grep-find
        efrit-tool-list-buffers
        efrit-tool-split-window)
  "List of available Elisp tools for Ollama.")

(defun ollama-elisp-execute-tool (tool-call)
  "Execute a tool call from Ollama.
TOOL-CALL is an alist with 'name' and 'arguments'."
  (let* ((name (alist-get 'name tool-call))
         (args (alist-get 'arguments tool-call))
         (result nil))
    
    (condition-case err
        (cond
         ;; Directory operations
         ((string= name "dired")
          (dired (alist-get 'directory args))
          (setq result (format "Opened directory: %s" (alist-get 'directory args))))
         
         ;; File operations
         ((string= name "find-file")
          (find-file (alist-get 'filename args))
          (setq result (format "Opened file: %s" (alist-get 'filename args))))
         
         ;; Load operations
         ((string= name "load-file")
          (load-file (alist-get 'file args))
          (setq result (format "Loaded file: %s" (alist-get 'file args))))
         
         ;; Search operations
         ((string= name "grep-find")
          (grep-find (alist-get 'command args))
          (setq result "Started grep search"))
         
         ;; Buffer operations
         ((string= name "list-buffers")
          (let ((buffers (mapcar #'buffer-name (buffer-list))))
            (setq result (json-encode buffers))))
         
         ;; Window operations
         ((string= name "split-window")
          (let ((direction (alist-get 'direction args)))
            (if (string= direction "horizontal")
                (split-window-below)
              (split-window-right))
            (setq result (format "Split window %s" direction))))
         
         ;; Unknown tool
         (t
          (error "Unknown tool: %s" name)))
      
      (error
       (setq result (format "Error executing %s: %s" name (error-message-string err)))))
    
    result))

(defun ollama-elisp-chat-with-tools (prompt)
  "Send PROMPT to Ollama with Elisp tools available."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((model . "llama3.1")
             (messages . [((role . "user") (content . ,prompt))])
             (tools . ,(vconcat ollama-elisp-tools-list))
             (stream . :false))))
         (response-buffer (url-retrieve-synchronously
                          "http://localhost:11434/api/chat")))
    
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((response (json-read))
               (message (alist-get 'message response))
               (tool-calls (alist-get 'tool_calls message)))
          
          ;; Execute any tool calls
          (when tool-calls
            (mapc (lambda (call)
                    (let ((result (ollama-elisp-execute-tool call)))
                      (message "Tool %s: %s" 
                               (alist-get 'name call) result)))
                  tool-calls))
          
          ;; Return the message content
          (alist-get 'content message))))))
```

## Testing Script

```elisp
;;; test-tool-calling.el --- Test Ollama tool calling

(defun test-ollama-tools ()
  "Test various tool calling scenarios."
  (interactive)
  
  ;; Test 1: List buffers
  (message "Test 1: Asking to list buffers...")
  (ollama-elisp-chat-with-tools "Can you list all the open buffers?")
  
  ;; Test 2: Open a file
  (message "Test 2: Asking to open init.el...")
  (ollama-elisp-chat-with-tools "Please open my init.el file")
  
  ;; Test 3: Search for something
  (message "Test 3: Asking to search for 'defun'...")
  (ollama-elisp-chat-with-tools "Search for all defun declarations in the current directory")
  
  ;; Test 4: Complex task
  (message "Test 4: Complex multi-tool task...")
  (ollama-elisp-chat-with-tools 
   "Open the experiments directory, then split the window and show me the buffer list"))

;; Run tests
(test-ollama-tools)
```

## Tracking Tool Calls

```elisp
(defvar ollama-tool-call-log nil
  "Log of all tool calls made.")

(defun ollama-log-tool-call (name args result)
  "Log a tool call for tracking."
  (push `((timestamp . ,(current-time-string))
          (name . ,name)
          (arguments . ,args)
          (result . ,result))
        ollama-tool-call-log))

(defun ollama-show-tool-history ()
  "Display tool call history."
  (interactive)
  (with-current-buffer (get-buffer-create "*Ollama Tool History*")
    (erase-buffer)
    (insert "=== Ollama Tool Call History ===\n\n")
    (dolist (call (reverse ollama-tool-call-log))
      (insert (format "[%s] %s\n"
                      (alist-get 'timestamp call)
                      (alist-get 'name call)))
      (insert (format "  Args: %S\n" (alist-get 'arguments call)))
      (insert (format "  Result: %s\n\n" (alist-get 'result call))))
    (display-buffer (current-buffer))))
```

## Benefits of Elisp Primitive Tools

1. **Direct Integration**: No wrapper functions needed - use Emacs functions directly
2. **Type Safety**: JSON schema ensures correct parameter types
3. **Discoverability**: LLM can understand what tools are available
4. **Composability**: LLM can chain multiple tools together
5. **Auditability**: All tool calls can be logged and reviewed

## Supported Models

According to Ollama documentation, these models support tool calling:
- Llama 3.1 (8b, 70b, 405b)
- Mistral Nemo
- Firefunction v2
- Command-R Plus

## Future Enhancements

- [ ] Add more Elisp primitives (occur, rgrep, project functions)
- [ ] Implement streaming tool calls when Ollama adds support
- [ ] Create tool validation framework
- [ ] Add permission system for dangerous operations
- [ ] Build tool composition patterns