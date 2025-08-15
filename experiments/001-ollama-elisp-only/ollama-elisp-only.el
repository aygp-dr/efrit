;;; ollama-elisp-only.el --- Ollama integration for pure Elisp responses -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Efrit Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (json "1.5"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/aygp-dr/efrit

;;; Commentary:
;; This module provides an interface to Ollama that enforces structured output
;; containing only valid Elisp code. The LLM is instructed to return responses
;; that can be directly evaluated by Emacs without any wrapper text.
;;
;; The key innovation is using Ollama's structured output capabilities with
;; very clear directives to ensure responses are pure, executable Elisp.

;;; Code:

(require 'json)
(require 'url)

(defgroup ollama-elisp nil
  "Ollama integration for Elisp-only responses."
  :group 'tools)

(defcustom ollama-elisp-host "http://localhost:11434"
  "Host URL for Ollama API."
  :type 'string
  :group 'ollama-elisp)

(defcustom ollama-elisp-model "qwen2.5-coder:7b"
  "Model to use for Elisp generation."
  :type 'string
  :group 'ollama-elisp)

(defcustom ollama-elisp-temperature 0.1
  "Temperature for response generation (lower = more deterministic)."
  :type 'number
  :group 'ollama-elisp)

(defvar ollama-elisp--system-prompt
  "You are an Elisp code generator integrated into Emacs.
CRITICAL RULES:
1. Your ENTIRE response must be valid Elisp code that can be executed with `eval'
2. NO explanations, NO comments outside of Elisp comments, NO markdown
3. Use Elisp functions to accomplish tasks (e.g., `directory-files' to list files)
4. If asked about the current directory, use `default-directory'
5. For file operations, use standard Elisp functions like `insert-file-contents', `find-file', etc.
6. Return results as Elisp data structures or use `message' for output
7. NEVER include backticks, code blocks markers, or any non-Elisp text
8. Your response will be directly passed to `eval' - any non-Elisp will cause an error

Examples:
User: 'list files in current directory'
Response: (directory-files default-directory nil \"^[^.]\")

User: 'what is the current directory'
Response: (message \"Current directory: %s\" default-directory)

User: 'read the first line of init.el'
Response: (with-temp-buffer
           (insert-file-contents \"~/.emacs.d/init.el\")
           (goto-char (point-min))
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position)))

Remember: ONLY valid Elisp, nothing else."
  "System prompt for enforcing Elisp-only responses.")

(defun ollama-elisp--make-request (prompt)
  "Make a request to Ollama with PROMPT, expecting Elisp response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((model . ,ollama-elisp-model)
             (prompt . ,prompt)
             (system . ,ollama-elisp--system-prompt)
             (stream . :false)
             (temperature . ,ollama-elisp-temperature)
             (format . ((type . "json")
                       (schema . ((type . "object")
                                 (properties . ((elisp . ((type . "string")
                                                         (description . "Valid Elisp code only"))))
                                 (required . ["elisp"])))))))))
         (response-buffer (url-retrieve-synchronously
                          (concat ollama-elisp-host "/api/generate")
                          nil t)))
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (response (json-read)))
          (kill-buffer response-buffer)
          (cdr (assoc 'response response)))))))

(defun ollama-elisp--extract-code (response)
  "Extract Elisp code from RESPONSE.
First tries to parse as JSON with 'elisp' field, then falls back to raw response."
  (condition-case nil
      (let* ((json-object-type 'alist)
             (parsed (json-read-from-string response)))
        (or (cdr (assoc 'elisp parsed)) response))
    (error response)))

(defun ollama-elisp-query (prompt)
  "Send PROMPT to Ollama and evaluate the Elisp response.
Returns the result of evaluating the generated Elisp code."
  (interactive "sPrompt: ")
  (message "Querying Ollama for Elisp...")
  (let* ((response (ollama-elisp--make-request prompt))
         (code (ollama-elisp--extract-code response)))
    (message "Received: %s" code)
    (condition-case err
        (eval (read code))
      (error
       (message "Error evaluating Elisp: %s\nCode was: %s" err code)
       nil))))

(defun ollama-elisp-query-insert (prompt)
  "Send PROMPT to Ollama and insert the generated Elisp at point."
  (interactive "sPrompt: ")
  (message "Querying Ollama for Elisp...")
  (let* ((response (ollama-elisp--make-request prompt))
         (code (ollama-elisp--extract-code response)))
    (insert code)))

(defun ollama-elisp-query-region (start end)
  "Send region from START to END as prompt to Ollama, replace with Elisp response."
  (interactive "r")
  (let* ((prompt (buffer-substring-no-properties start end))
         (response (ollama-elisp--make-request prompt))
         (code (ollama-elisp--extract-code response)))
    (delete-region start end)
    (insert code)))

;; Advanced function for context-aware queries
(defun ollama-elisp-context-query (prompt)
  "Query Ollama with PROMPT and current buffer context.
Includes information about current buffer, mode, and point."
  (interactive "sPrompt: ")
  (let* ((context-prompt
          (format "Context: buffer=%s, mode=%s, point=%d, line=%d\nRequest: %s"
                  (buffer-name)
                  major-mode
                  (point)
                  (line-number-at-pos)
                  prompt))
         (response (ollama-elisp--make-request context-prompt))
         (code (ollama-elisp--extract-code response)))
    (message "Executing: %s" code)
    (eval (read code))))

;; Helper function to test connectivity
(defun ollama-elisp-test-connection ()
  "Test connection to Ollama server."
  (interactive)
  (condition-case err
      (let ((response (ollama-elisp--make-request "Generate: (message \"Ollama connected\")")))
        (if response
            (message "Successfully connected to Ollama at %s" ollama-elisp-host)
          (message "Failed to get response from Ollama")))
    (error (message "Connection error: %s" err))))

(provide 'ollama-elisp-only)
;;; ollama-elisp-only.el ends here