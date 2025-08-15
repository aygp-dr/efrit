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
