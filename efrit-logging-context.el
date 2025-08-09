;;; efrit-logging-context.el --- Context persistence for Efrit logging -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, logging
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This module extends efrit-logging to persist project and context information
;; to enable better self-inspection and analysis capabilities.

;;; Code:

(require 'efrit-logging)
(require 'json)

;;; Customization

(defcustom efrit-logging-save-project-context t
  "Whether to save project context information in logs."
  :type 'boolean
  :group 'efrit-logging)

(defcustom efrit-logging-context-file ".efrit/project-context.json"
  "File to save persistent project context relative to project root."
  :type 'string
  :group 'efrit-logging)

;;; Project context persistence

(defun efrit-logging-save-project-context (project-info)
  "Save PROJECT-INFO to a persistent file in the project directory."
  (when (and efrit-logging-save-project-context
             project-info
             (alist-get 'root project-info))
    (let* ((root (alist-get 'root project-info))
           (context-file (expand-file-name efrit-logging-context-file root))
           (context-dir (file-name-directory context-file))
           (enhanced-info (efrit-logging--enhance-project-info project-info)))
      (condition-case err
          (progn
            (make-directory context-dir t)
            (with-temp-file context-file
              (insert (json-encode-plist enhanced-info)))
            (efrit-logging-log 'debug 'context 
                              (format "Saved project context to %s" context-file)))
        (error
         (efrit-logging-log 'warning 'context 
                           (format "Failed to save project context: %s" 
                                   (error-message-string err))))))))

(defun efrit-logging--enhance-project-info (project-info)
  "Enhance PROJECT-INFO with additional metadata."
  (append project-info
          `((last-updated . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
            (emacs-version . ,emacs-version)
            (system-type . ,system-type)
            (efrit-session . ,efrit-logging--session-id)
            (statistics . ,(efrit-logging--get-project-statistics 
                           (alist-get 'root project-info))))))

(defun efrit-logging--get-project-statistics (root)
  "Get statistics for project at ROOT."
  (when root
    (condition-case nil
        `((total-files . ,(length (directory-files-recursively 
                                   root "\\(\\.el\\|\\.py\\|\\.js\\)$" nil t)))
          (elisp-files . ,(length (directory-files-recursively 
                                   root "\\.el$" nil t)))
          (last-modified . ,(file-attribute-modification-time 
                             (file-attributes root))))
      (error nil))))

(defun efrit-logging-load-project-context (root)
  "Load saved project context for ROOT directory."
  (let ((context-file (expand-file-name efrit-logging-context-file root)))
    (when (file-exists-p context-file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents context-file)
            (json-read))
        (error nil)))))

;;; Extended request logging with context

(defun efrit-logging-log-request-with-context (endpoint request-data context)
  "Log request with full CONTEXT including project info."
  (when efrit-logging-enabled
    (efrit-logging--ensure-directories)
    (cl-incf efrit-logging--request-counter)
    (let* ((request-id (format "%s-%04d" 
                                efrit-logging--session-id
                                efrit-logging--request-counter))
           (project-info (alist-get 'project context))
           (entry (json-encode
                   `((type . "request-with-context")
                     (id . ,request-id)
                     (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                     (session_id . ,efrit-logging--session-id)
                     (endpoint . ,endpoint)
                     (data . ,request-data)
                     (context . ,context)
                     (project . ,project-info)))))
      ;; Save to both request log and context log
      (efrit-logging--append-to-file 
       (efrit-logging--get-log-file 'request)
       entry)
      ;; Also save project context if present
      (when project-info
        (efrit-logging-save-project-context project-info))
      request-id)))

;;; Context analysis functions

(defun efrit-logging-analyze-project-contexts ()
  "Analyze all saved project contexts across the system."
  (interactive)
  (let ((contexts nil)
        (efrit-dir (expand-file-name efrit-logging-directory)))
    ;; Find all project-context.json files
    (dolist (file (directory-files-recursively efrit-dir "project-context\\.json$"))
      (when-let ((context (efrit-logging--read-json-file file)))
        (push (cons file context) contexts)))
    
    ;; Display analysis
    (with-current-buffer (get-buffer-create "*Efrit Project Contexts*")
      (erase-buffer)
      (insert "Efrit Project Context Analysis\n")
      (insert "==============================\n\n")
      (if contexts
          (dolist (ctx contexts)
            (let ((file (car ctx))
                  (data (cdr ctx)))
              (insert (format "File: %s\n" file))
              (insert (format "  Root: %s\n" (alist-get 'root data)))
              (insert (format "  Type: %s\n" (alist-get 'type data)))
              (insert (format "  VCS: %s\n" (alist-get 'vc-backend data)))
              (insert (format "  Last Updated: %s\n" (alist-get 'last-updated data)))
              (insert "\n")))
        (insert "No project contexts found.\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer "*Efrit Project Contexts*")))

(defun efrit-logging--read-json-file (file)
  "Read JSON from FILE safely."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (json-read))
    (error nil)))

;;; Integration with agent context

(defun efrit-logging-get-full-context ()
  "Get the full context including saved project information."
  (let* ((current-project (when (fboundp 'efrit-agent--get-project-info)
                            (efrit-agent--get-project-info)))
         (saved-context (when current-project
                         (efrit-logging-load-project-context 
                          (alist-get 'root current-project)))))
    `((current . ,current-project)
      (saved . ,saved-context)
      (session . ,efrit-logging--session-id)
      (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))))

;;; Session context tracking

(defvar efrit-logging--session-contexts nil
  "List of contexts used in this session.")

(defun efrit-logging-track-context (context)
  "Track CONTEXT usage in current session."
  (when efrit-logging-enabled
    (push `((timestamp . ,(current-time))
            (context . ,context))
          efrit-logging--session-contexts)))

(defun efrit-logging-save-session-contexts ()
  "Save all session contexts to log."
  (when (and efrit-logging-enabled
             efrit-logging--session-contexts)
    (let ((session-file (expand-file-name 
                         (format "sessions/%s-contexts.jsonl" 
                                 efrit-logging--session-id)
                         (expand-file-name efrit-logging-directory))))
      (make-directory (file-name-directory session-file) t)
      (with-temp-file session-file
        (dolist (ctx (reverse efrit-logging--session-contexts))
          (insert (json-encode ctx) "\n"))))))

;;; Hooks for automatic context saving

(defun efrit-logging-context-initialize ()
  "Initialize context logging."
  (add-hook 'kill-emacs-hook #'efrit-logging-save-session-contexts))

(provide 'efrit-logging-context)
;;; efrit-logging-context.el ends here