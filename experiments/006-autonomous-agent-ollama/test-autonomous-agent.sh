#!/usr/bin/env sh
# Test Efrit Autonomous Agent Mode with Ollama
# Both curl and Emacs examples for self-awareness prompting

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

OLLAMA_HOST="${OLLAMA_HOST:-http://localhost:11434}"
MODEL="qwen2.5-coder:7b"

echo "${BLUE}=== Efrit Autonomous Agent Mode Test ===${NC}"
echo "Testing self-aware agent with action planning"
echo ""

# ==============================
# PART 1: CURL EXAMPLES
# ==============================

echo "${YELLOW}PART 1: Testing with curl${NC}"
echo ""

# Self-awareness prompt template
AGENT_SYSTEM_PROMPT="You are Efrit, an autonomous Emacs agent. You maintain a TODO list and work systematically toward goals. You have access to elisp evaluation, shell commands, and file operations. You can consult other AI models for complex reasoning.

Respond with JSON containing:
- status: planning|executing|reflecting|stuck|complete
- todos: array of todo items with id, status, priority
- next_action: object with type (eval|shell|file_read|ai_consult|user_input), content, and rationale
- self_assessment: brief statement of current understanding
- completion_estimate: estimate of remaining actions needed"

# Test 1: Planning Phase
echo "${GREEN}Test 1: Initial Planning${NC}"
cat > /tmp/agent-planning.json <<EOF
{
  "model": "$MODEL",
  "prompt": "Current Goal: Upgrade my chatgpt-shell package\\nSession State: Just started, no actions taken yet\\nAvailable Tools: elisp eval, shell commands, file operations\\n\\nWhat is your initial plan?",
  "system": "$AGENT_SYSTEM_PROMPT",
  "stream": false,
  "format": "json",
  "temperature": 0.3
}
EOF

echo "Request:"
echo "Goal: Upgrade chatgpt-shell package"
echo ""
echo "Response:"
curl -s -X POST "$OLLAMA_HOST/api/generate" \
    -H "Content-Type: application/json" \
    -d @/tmp/agent-planning.json | jq -r '.response' | jq .
echo ""

# Test 2: Action Execution
echo "${GREEN}Test 2: Action Execution Signal${NC}"
cat > /tmp/agent-action.json <<EOF
{
  "model": "$MODEL",
  "prompt": "Current Goal: Check Emacs package versions\\nSession State: Need to list installed packages\\nAvailable Tools: elisp eval, shell commands\\n\\nGenerate next action to list Emacs packages.",
  "system": "$AGENT_SYSTEM_PROMPT",
  "stream": false,
  "format": "json",
  "temperature": 0.3
}
EOF

echo "Response:"
curl -s -X POST "$OLLAMA_HOST/api/generate" \
    -H "Content-Type: application/json" \
    -d @/tmp/agent-action.json | jq -r '.response' | jq .
echo ""

# Test 3: Pure Elisp Generation (for eval actions)
echo "${GREEN}Test 3: Elisp Code Generation for Actions${NC}"
cat > /tmp/elisp-action.json <<EOF
{
  "model": "$MODEL",
  "prompt": "Generate Elisp code to check if straight.el is installed and get its version",
  "system": "Return ONLY valid Elisp code that can be executed with eval. No explanations, no markdown, just pure Elisp.",
  "stream": false,
  "temperature": 0.1
}
EOF

echo "Response (pure Elisp):"
curl -s -X POST "$OLLAMA_HOST/api/generate" \
    -H "Content-Type: application/json" \
    -d @/tmp/elisp-action.json | jq -r '.response'
echo ""

# ==============================
# PART 2: EMACS ELISP EXAMPLES
# ==============================

echo "${YELLOW}PART 2: Emacs Elisp Implementation${NC}"
echo ""

# Create Elisp test file
cat > /tmp/test-autonomous-agent.el <<'ELISP'
;;; test-autonomous-agent.el --- Test Efrit Autonomous Agent Mode

(require 'json)
(require 'url)

(defvar efrit-ollama-host "http://localhost:11434"
  "Ollama API host.")

(defvar efrit-ollama-model "qwen2.5-coder:7b"
  "Model to use for agent reasoning.")

(defun efrit-agent-system-prompt ()
  "Return the system prompt for autonomous agent mode."
  "You are Efrit, an autonomous Emacs agent. You maintain a TODO list and work systematically toward goals. You have access to elisp evaluation, shell commands, and file operations.

Respond with JSON containing:
- status: planning|executing|reflecting|stuck|complete
- todos: array of todo items with id, status, priority
- next_action: object with type (eval|shell|file_read), content, and rationale
- self_assessment: brief statement of current understanding
- completion_estimate: estimate of remaining actions needed")

(defun efrit-agent-query (goal context)
  "Query Ollama for next agent action given GOAL and CONTEXT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((model . ,efrit-ollama-model)
             (prompt . ,(format "Current Goal: %s\nSession State: %s\nAvailable Tools: elisp eval, shell commands, file operations\n\nWhat is your next action?"
                                goal context))
             (system . ,(efrit-agent-system-prompt))
             (stream . :false)
             (format . "json")
             (temperature . 0.3))))
         (response-buffer (url-retrieve-synchronously
                           (concat efrit-ollama-host "/api/generate"))))
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (let* ((json-object-type 'plist)
               (response (json-read))
               (agent-response (json-read-from-string (plist-get response :response))))
          (kill-buffer response-buffer)
          agent-response)))))

(defun efrit-agent-execute-action (action-plist)
  "Execute an action from the agent's ACTION-PLIST."
  (let ((type (plist-get action-plist :type))
        (content (plist-get action-plist :content)))
    (cond
     ((string= type "eval")
      (eval (read content)))
     ((string= type "shell")
      (shell-command-to-string content))
     ((string= type "file_read")
      (with-temp-buffer
        (insert-file-contents content)
        (buffer-string)))
     (t (error "Unknown action type: %s" type)))))

(defun efrit-agent-test-planning ()
  "Test the planning phase of autonomous agent."
  (interactive)
  (let ((response (efrit-agent-query 
                   "Upgrade chatgpt-shell package"
                   "Just started, no actions taken yet")))
    (message "Agent Planning Response:")
    (message "Status: %s" (plist-get response :status))
    (message "TODOs: %s" (plist-get response :todos))
    (message "Next Action: %s" (plist-get response :next_action))
    (message "Assessment: %s" (plist-get response :self_assessment))
    response))

(defun efrit-agent-test-execution ()
  "Test action execution."
  (interactive)
  (let* ((response (efrit-agent-query 
                    "Check Emacs version"
                    "Need to get system information"))
         (action (plist-get response :next_action)))
    (when action
      (message "Executing action: %s" (plist-get action :type))
      (let ((result (efrit-agent-execute-action action)))
        (message "Result: %s" result)
        result))))

(defun efrit-agent-generate-elisp (task)
  "Generate pure Elisp code for TASK."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((model . ,efrit-ollama-model)
             (prompt . ,task)
             (system . "Return ONLY valid Elisp code that can be executed with eval. No explanations, no markdown, just pure Elisp.")
             (stream . :false)
             (temperature . 0.1))))
         (response-buffer (url-retrieve-synchronously
                           (concat efrit-ollama-host "/api/generate"))))
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (let* ((json-object-type 'plist)
               (response (json-read))
               (elisp-code (plist-get response :response)))
          (kill-buffer response-buffer)
          elisp-code)))))

;; Interactive demo
(defun efrit-agent-demo ()
  "Run a demo of the autonomous agent."
  (interactive)
  (message "=== Efrit Autonomous Agent Demo ===")
  
  ;; Test 1: Planning
  (message "\n1. Testing Planning Phase...")
  (efrit-agent-test-planning)
  
  ;; Test 2: Execution
  (message "\n2. Testing Action Execution...")
  (efrit-agent-test-execution)
  
  ;; Test 3: Pure Elisp Generation
  (message "\n3. Testing Elisp Generation...")
  (let ((elisp-code (efrit-agent-generate-elisp "get current buffer name")))
    (message "Generated: %s" elisp-code)
    (message "Evaluated: %s" (eval (read elisp-code))))
  
  (message "\n=== Demo Complete ==="))

(provide 'test-autonomous-agent)
ELISP

echo "Created test-autonomous-agent.el"
echo ""

echo "${GREEN}Testing Emacs Integration${NC}"
echo "To test in Emacs, run:"
echo '  emacs --batch -l /tmp/test-autonomous-agent.el --eval "(efrit-agent-demo)"'
echo ""

# Actually run the Emacs test
echo "${BLUE}Running Emacs batch test...${NC}"
emacs --batch -l /tmp/test-autonomous-agent.el \
      --eval "(progn
                (message \"Testing agent planning...\")
                (condition-case err
                    (let ((response (efrit-agent-query 
                                     \"List files in current directory\"
                                     \"Starting fresh\")))
                      (message \"Response: %S\" response))
                  (error (message \"Error: %S\" err))))" 2>&1

echo ""
echo "${GREEN}=== Test Complete ===${NC}"
echo ""
echo "Summary:"
echo "1. curl examples show how to interact with Ollama for agent actions"
echo "2. Agent responses include status, todos, and next_action"
echo "3. Pure Elisp can be generated for eval-type actions"
echo "4. Emacs integration uses url.el for HTTP requests"
echo "5. Actions can be eval, shell, or file_read types"