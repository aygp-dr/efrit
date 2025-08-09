;;; context-persistence-demo.el --- Demo of context persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; This demonstrates how Efrit saves and uses project context information.

;;; Code:

(require 'efrit-logging)
(require 'efrit-logging-context)

(defun efrit-demo-show-saved-context ()
  "Show the saved project context for the current project."
  (interactive)
  (let* ((project-info (when (fboundp 'efrit-agent--get-project-info)
                         (efrit-agent--get-project-info)))
         (root (alist-get 'root project-info))
         (saved-context (when root
                         (efrit-logging-load-project-context root))))
    (if saved-context
        (with-output-to-temp-buffer "*Efrit Saved Context*"
          (princ "Saved Project Context\n")
          (princ "====================\n\n")
          (princ (pp-to-string saved-context)))
      (message "No saved context found for this project"))))

(defun efrit-demo-save-current-context ()
  "Save the current project context."
  (interactive)
  (let ((project-info (when (fboundp 'efrit-agent--get-project-info)
                        (efrit-agent--get-project-info))))
    (if project-info
        (progn
          (efrit-logging-save-project-context project-info)
          (message "Project context saved to .efrit/project-context.json"))
      (message "No project detected"))))

(defun efrit-demo-compare-contexts ()
  "Compare current context with saved context."
  (interactive)
  (let* ((current (when (fboundp 'efrit-agent--get-project-info)
                    (efrit-agent--get-project-info)))
         (saved (when current
                 (efrit-logging-load-project-context 
                  (alist-get 'root current)))))
    (with-output-to-temp-buffer "*Context Comparison*"
      (princ "Context Comparison\n")
      (princ "==================\n\n")
      (princ "CURRENT CONTEXT:\n")
      (princ (pp-to-string current))
      (princ "\n\nSAVED CONTEXT:\n")
      (if saved
          (princ (pp-to-string saved))
        (princ "No saved context found")))))

;; Example of how logging would capture full context
(defun efrit-demo-log-with-context (message)
  "Log MESSAGE with full project context."
  (interactive "sMessage to log: ")
  (let* ((context (efrit-logging-get-full-context))
         (project-info (alist-get 'current context)))
    ;; Save project context if available
    (when project-info
      (efrit-logging-save-project-context project-info))
    ;; Log the message with context
    (efrit-logging-log 'info 'demo message context)
    (message "Logged: %s (with context)" message)))

;; Show what would be persisted
(defun efrit-demo-show-persistence-structure ()
  "Show the structure of persisted data."
  (interactive)
  (let ((structure
         '((".efrit/"
            ("project-context.json" . "Project metadata and statistics")
            ("requests/" . "API request logs in JSONL format")
            ("responses/" . "API response logs in JSONL format")
            ("sessions/" . "Session-specific logs and contexts")
            ("errors/" . "Error logs for debugging"))))
        (buffer (get-buffer-create "*Efrit Persistence Structure*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Persistence Structure\n")
      (insert "===========================\n\n")
      (insert "Project Root:\n")
      (insert "  .efrit/\n")
      (insert "    project-context.json  - Project metadata, statistics, history\n\n")
      (insert "User Home (~/.efrit/):\n")
      (insert "  requests/              - All API requests across projects\n")
      (insert "  responses/             - All API responses\n")
      (insert "  sessions/              - Session logs and contexts\n")
      (insert "  errors/                - Error logs\n\n")
      (insert "Example project-context.json:\n")
      (insert "{\n")
      (insert "  \"root\": \"/path/to/project\",\n")
      (insert "  \"type\": \"git\",\n")
      (insert "  \"vc-backend\": \"Git\",\n")
      (insert "  \"last-updated\": \"2025-01-09T10:00:00-0800\",\n")
      (insert "  \"statistics\": {\n")
      (insert "    \"total-files\": 42,\n")
      (insert "    \"elisp-files\": 10\n")
      (insert "  },\n")
      (insert "  \"efrit-session\": \"20250109-100000-abc123\"\n")
      (insert "}\n"))
    (switch-to-buffer buffer)))

(provide 'context-persistence-demo)
;;; context-persistence-demo.el ends here