;;; ollama-elisp-tools.el --- Ollama tool calling with Elisp primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Efrit Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (json "1.5"))
;; Keywords: tools, ai, ollama
;; URL: https://github.com/aygp-dr/efrit

;;; Commentary:
;; This module provides Ollama tool calling support using native Elisp
;; primitives. It exposes Emacs functions like dired, find-file, grep-find
;; as tools that can be called by LLMs through the Ollama API.

;;; Code:

(require 'json)
(require 'url)
(require 'dired)
(require 'grep)

;;; Tool Definitions

(defvar ollama-tool-dired
  '((type . "function")
    (function . ((name . "dired")
                 (description . "Open directory listing in Dired mode to browse files")
                 (parameters . ((type . "object")
                               (properties . ((directory . ((type . "string")
                                                           (description . "Directory path to open (absolute or relative)")))))
                               (required . ["directory"]))))))

(defvar ollama-tool-find-file
  '((type . "function")
    (function . ((name . "find-file")
                 (description . "Open a file in Emacs for viewing or editing")
                 (parameters . ((type . "object")
                               (properties . ((filename . ((type . "string")
                                                          (description . "Path to the file to open")))))
                               (required . ["filename"]))))))

(defvar ollama-tool-load-file
  '((type . "function")
    (function . ((name . "load-file")
                 (description . "Load and evaluate an Elisp file")
                 (parameters . ((type . "object")
                               (properties . ((file . ((type . "string")
                                                      (description . "Path to the Elisp file to load")))))
                               (required . ["file"]))))))

(defvar ollama-tool-grep-find
  '((type . "function")
    (function . ((name . "grep-find")
                 (description . "Search for a pattern in files using grep")
                 (parameters . ((type . "object")
                               (properties . ((pattern . ((type . "string")
                                                         (description . "Search pattern or regex")))
                                            (directory . ((type . "string")
                                                        (description . "Directory to search in")
                                                        (default . ".")))
                                            (file-pattern . ((type . "string")
                                                           (description . "File pattern to search (e.g., *.el)")
                                                           (default . "*")))))
                               (required . ["pattern"]))))))

(defvar ollama-tool-list-buffers
  '((type . "function")
    (function . ((name . "list-buffers")
                 (description . "List all currently open buffers in Emacs")
                 (parameters . ((type . "object")
                               (properties . ((files-only . ((type . "boolean")
                                                            (description . "If true, list only buffers visiting files")
                                                            (default . :false)))))
                               (required . []))))))

(defvar ollama-tool-switch-to-buffer
  '((type . "function")
    (function . ((name . "switch-to-buffer")
                 (description . "Switch to a specific buffer by name")
                 (parameters . ((type . "object")
                               (properties . ((buffer-name . ((type . "string")
                                                             (description . "Name of the buffer to switch to")))))
                               (required . ["buffer-name"]))))))

(defvar ollama-tool-save-buffer
  '((type . "function")
    (function . ((name . "save-buffer")
                 (description . "Save the current buffer to its file")
                 (parameters . ((type . "object")
                               (properties . ())
                               (required . []))))))

(defvar ollama-tool-eval-region
  '((type . "function")
    (function . ((name . "eval-region")
                 (description . "Evaluate Elisp code in a region or string")
                 (parameters . ((type . "object")
                               (properties . ((code . ((type . "string")
                                                      (description . "Elisp code to evaluate")))))
                               (required . ["code"]))))))

(defvar ollama-tool-describe-function
  '((type . "function")
    (function . ((name . "describe-function")
                 (description . "Get documentation for an Elisp function")
                 (parameters . ((type . "object")
                               (properties . ((function-name . ((type . "string")
                                                               (description . "Name of the function to describe")))))
                               (required . ["function-name"]))))))

;;; Tool Registry

(defvar ollama-elisp-tools-list
  (list ollama-tool-dired
        ollama-tool-find-file
        ollama-tool-load-file
        ollama-tool-grep-find
        ollama-tool-list-buffers
        ollama-tool-switch-to-buffer
        ollama-tool-save-buffer
        ollama-tool-eval-region
        ollama-tool-describe-function)
  "List of available Elisp tools for Ollama.")

;;; Tool Call Tracking

(defvar ollama-tool-call-log nil
  "Log of all tool calls made.")

(defvar ollama-tool-call-counter 0
  "Counter for tool calls.")

(defun ollama-log-tool-call (name args result)
  "Log a tool call with NAME, ARGS, and RESULT."
  (setq ollama-tool-call-counter (1+ ollama-tool-call-counter))
  (push `((id . ,ollama-tool-call-counter)
          (timestamp . ,(current-time-string))
          (name . ,name)
          (arguments . ,args)
          (result . ,result))
        ollama-tool-call-log))

;;; Tool Execution

(defun ollama-elisp-execute-tool (tool-call)
  "Execute a TOOL-CALL from Ollama.
TOOL-CALL is an alist with 'name' and 'arguments'."
  (let* ((name (alist-get 'name tool-call))
         (args (alist-get 'arguments tool-call))
         (result nil))
    
    (condition-case err
        (cond
         ;; Directory operations
         ((string= name "dired")
          (let ((dir (expand-file-name (or (alist-get 'directory args) "."))))
            (dired dir)
            (setq result (format "Opened directory: %s with %d entries" 
                               dir (length (directory-files dir))))))
         
         ;; File operations
         ((string= name "find-file")
          (let ((file (expand-file-name (alist-get 'filename args))))
            (find-file file)
            (setq result (format "Opened file: %s (%d bytes)" 
                               file (buffer-size)))))
         
         ;; Load operations
         ((string= name "load-file")
          (let ((file (expand-file-name (alist-get 'file args))))
            (load-file file)
            (setq result (format "Loaded Elisp file: %s" file))))
         
         ;; Search operations
         ((string= name "grep-find")
          (let* ((pattern (alist-get 'pattern args))
                 (dir (or (alist-get 'directory args) "."))
                 (file-pat (or (alist-get 'file-pattern args) "*"))
                 (command (format "find %s -name '%s' -exec grep -nH '%s' {} +"
                                dir file-pat pattern)))
            (grep-find command)
            (setq result (format "Searching for '%s' in %s/%s" 
                               pattern dir file-pat))))
         
         ;; Buffer operations
         ((string= name "list-buffers")
          (let* ((files-only (alist-get 'files-only args))
                 (buffers (if files-only
                             (cl-remove-if-not #'buffer-file-name (buffer-list))
                           (buffer-list)))
                 (buffer-info (mapcar (lambda (buf)
                                       `((name . ,(buffer-name buf))
                                         (size . ,(buffer-size buf))
                                         (file . ,(buffer-file-name buf))
                                         (mode . ,(with-current-buffer buf
                                                   (symbol-name major-mode)))))
                                     buffers)))
            (setq result (json-encode buffer-info))))
         
         ;; Buffer switching
         ((string= name "switch-to-buffer")
          (let ((buf-name (alist-get 'buffer-name args)))
            (switch-to-buffer buf-name)
            (setq result (format "Switched to buffer: %s" buf-name))))
         
         ;; Save operations
         ((string= name "save-buffer")
          (save-buffer)
          (setq result (format "Saved buffer: %s" (buffer-name))))
         
         ;; Evaluation
         ((string= name "eval-region")
          (let* ((code (alist-get 'code args))
                 (eval-result (eval (read code))))
            (setq result (format "Evaluated: %S => %S" code eval-result))))
         
         ;; Documentation
         ((string= name "describe-function")
          (let* ((func-name (alist-get 'function-name args))
                 (func-sym (intern func-name))
                 (doc (documentation func-sym)))
            (setq result (or doc (format "No documentation for %s" func-name)))))
         
         ;; Unknown tool
         (t
          (error "Unknown tool: %s" name)))
      
      ;; Error handling
      (error
       (setq result (format "Error executing %s: %s" 
                          name (error-message-string err)))))
    
    ;; Log the tool call
    (ollama-log-tool-call name args result)
    
    result))

;;; API Integration

(defun ollama-elisp-chat-with-tools (prompt &optional model)
  "Send PROMPT to Ollama with Elisp tools available.
Use MODEL (default: llama3.1) for the chat."
  (interactive "sPrompt: ")
  (let* ((model (or model "llama3.1"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((model . ,model)
             (messages . [((role . "user") (content . ,prompt))])
             (tools . ,(vconcat ollama-elisp-tools-list))
             (stream . :false))))
         (response-buffer (url-retrieve-synchronously
                          "http://localhost:11434/api/chat"
                          nil t)))
    
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$")
        (let* ((json-object-type 'alist)
               (response (json-read))
               (message (alist-get 'message response))
               (tool-calls (alist-get 'tool_calls message))
               (content (alist-get 'content message)))
          
          (kill-buffer response-buffer)
          
          ;; Execute any tool calls
          (when tool-calls
            (message "Executing %d tool calls..." (length tool-calls))
            (dolist (call tool-calls)
              (let ((result (ollama-elisp-execute-tool call)))
                (message "Tool [%s]: %s" 
                        (alist-get 'name call) result))))
          
          ;; Return the response
          content)))))

;;; Tool History UI

(defun ollama-show-tool-history ()
  "Display tool call history in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Ollama Tool History*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "═══ Ollama Tool Call History ═══\n")
      (insert (format "Total calls: %d\n\n" ollama-tool-call-counter))
      
      (dolist (call (reverse ollama-tool-call-log))
        (insert (format "┌─ Call #%d at %s\n"
                       (alist-get 'id call)
                       (alist-get 'timestamp call)))
        (insert (format "├─ Tool: %s\n" (alist-get 'name call)))
        (insert (format "├─ Args: %S\n" (alist-get 'arguments call)))
        (insert (format "└─ Result: %s\n\n" (alist-get 'result call))))
      
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer (current-buffer))))

(defun ollama-clear-tool-history ()
  "Clear the tool call history."
  (interactive)
  (setq ollama-tool-call-log nil)
  (setq ollama-tool-call-counter 0)
  (message "Tool call history cleared"))

;;; Testing Functions

(defun ollama-test-tool-calling ()
  "Test various tool calling scenarios."
  (interactive)
  (let ((test-cases
         '("List all open buffers"
           "Open the file ~/.emacs.d/init.el"
           "Search for 'defun' in all elisp files"
           "Show me the documentation for the function 'mapcar'"
           "Save the current buffer")))
    
    (dolist (test test-cases)
      (message "\nTest: %s" test)
      (let ((response (ollama-elisp-chat-with-tools test)))
        (message "Response: %s" response))
      (sit-for 1))))

(provide 'ollama-elisp-tools)
;;; ollama-elisp-tools.el ends here