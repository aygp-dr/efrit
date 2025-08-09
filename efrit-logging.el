;;; efrit-logging.el --- Logging system for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, logging
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This module provides comprehensive logging for Efrit, including:
;; - Request/response logging to ~/.efrit directory
;; - JSONL format for structured data
;; - Log rotation and retention
;; - Self-inspection capabilities
;; - Debug and error tracking

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup efrit-logging nil
  "Logging configuration for Efrit."
  :group 'efrit
  :prefix "efrit-logging-")

(defcustom efrit-logging-enabled nil
  "Whether to enable logging for Efrit sessions."
  :type 'boolean
  :group 'efrit-logging)

(defcustom efrit-logging-directory "~/.efrit"
  "Directory for Efrit logs."
  :type 'directory
  :group 'efrit-logging)

(defcustom efrit-logging-level 'info
  "Logging level for Efrit."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warning)
                 (const :tag "Error" error))
  :group 'efrit-logging)

(defcustom efrit-logging-retention-days 30
  "Number of days to retain log files."
  :type 'integer
  :group 'efrit-logging)

(defcustom efrit-logging-max-file-size-mb 10
  "Maximum size of a log file in megabytes before rotation."
  :type 'integer
  :group 'efrit-logging)

;;; Internal variables

(defvar efrit-logging--request-counter 0
  "Counter for generating request IDs.")

(defvar efrit-logging--session-id nil
  "Current session ID.")

;;; Directory management

(defun efrit-logging--ensure-directories ()
  "Ensure logging directories exist."
  (let ((log-dir (expand-file-name efrit-logging-directory)))
    (dolist (subdir '("requests" "responses" "errors" "sessions"))
      (make-directory (expand-file-name subdir log-dir) t))))

(defun efrit-logging--get-log-file (type)
  "Get the log file path for TYPE."
  (let* ((log-dir (expand-file-name efrit-logging-directory))
         (date (format-time-string "%Y-%m-%d"))
         (subdir (pcase type
                   ('request "requests")
                   ('response "responses")
                   ('error "errors")
                   ('session "sessions")
                   (_ "misc"))))
    (expand-file-name (format "%s/%s.jsonl" subdir date) log-dir)))

;;; Session management

(defun efrit-logging--init-session ()
  "Initialize a new logging session."
  (setq efrit-logging--session-id 
        (format "%s-%s" 
                (format-time-string "%Y%m%d%H%M%S")
                (random 10000)))
  (setq efrit-logging--request-counter 0)
  (when efrit-logging-enabled
    (efrit-logging--ensure-directories)
    (efrit-logging--log-session-start)))

(defun efrit-logging--log-session-start ()
  "Log the start of a session."
  (let ((entry (json-encode
                `((type . "session_start")
                  (session_id . ,efrit-logging--session-id)
                  (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                  (emacs_version . ,emacs-version)
                  (system_type . ,system-type)))))
    (efrit-logging--append-to-file 
     (efrit-logging--get-log-file 'session)
     entry)))

;;; Core logging functions

(defun efrit-logging--append-to-file (file content)
  "Append CONTENT to FILE as a new line."
  (with-temp-buffer
    (insert content "\n")
    (append-to-file (point-min) (point-max) file)))

(defun efrit-logging--should-log-p (level)
  "Check if LEVEL should be logged based on current settings."
  (and efrit-logging-enabled
       (let ((levels '(debug info warning error))
             (current-level efrit-logging-level))
         (>= (cl-position level levels)
             (cl-position current-level levels)))))

(defun efrit-logging-log (level component message &optional data)
  "Log a MESSAGE at LEVEL from COMPONENT with optional DATA."
  (when (efrit-logging--should-log-p level)
    (efrit-logging--ensure-directories)
    (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (entry (json-encode
                   `((type . "log")
                     (level . ,level)
                     (component . ,component)
                     (message . ,message)
                     (timestamp . ,timestamp)
                     (session_id . ,efrit-logging--session-id)
                     ,@(when data `((data . ,data))))))
           (file (if (eq level 'error)
                     (efrit-logging--get-log-file 'error)
                   (efrit-logging--get-log-file 'session))))
      (efrit-logging--append-to-file file entry))))

;;; Request/Response logging

(defun efrit-logging-log-request (endpoint request-data)
  "Log an API request to ENDPOINT with REQUEST-DATA."
  (when efrit-logging-enabled
    (efrit-logging--ensure-directories)
    (cl-incf efrit-logging--request-counter)
    (let* ((request-id (format "%s-%04d" 
                                efrit-logging--session-id
                                efrit-logging--request-counter))
           (entry (json-encode
                   `((type . "request")
                     (id . ,request-id)
                     (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                     (session_id . ,efrit-logging--session-id)
                     (endpoint . ,endpoint)
                     (data . ,request-data)))))
      (efrit-logging--append-to-file 
       (efrit-logging--get-log-file 'request)
       entry)
      request-id)))

(defun efrit-logging-log-response (request-id response-data &optional error)
  "Log an API response for REQUEST-ID with RESPONSE-DATA or ERROR."
  (when efrit-logging-enabled
    (efrit-logging--ensure-directories)
    (let* ((entry (json-encode
                   `((type . "response")
                     (request_id . ,request-id)
                     (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                     (session_id . ,efrit-logging--session-id)
                     ,@(if error
                           `((error . ,error))
                         `((data . ,response-data)))))))
      (efrit-logging--append-to-file 
       (efrit-logging--get-log-file 'response)
       entry))))

;;; Log rotation and cleanup

(defun efrit-logging-rotate-logs ()
  "Rotate log files that exceed size limit."
  (interactive)
  (let ((log-dir (expand-file-name efrit-logging-directory))
        (max-size (* efrit-logging-max-file-size-mb 1024 1024)))
    (dolist (file (directory-files-recursively log-dir "\\.jsonl$"))
      (when (> (file-attribute-size (file-attributes file)) max-size)
        (let ((rotated-file (format "%s.%s" 
                                     file 
                                     (format-time-string "%Y%m%d%H%M%S"))))
          (rename-file file rotated-file)
          (efrit-logging-log 'info 'logging 
                             (format "Rotated log file: %s" 
                                     (file-name-nondirectory file))))))))

(defun efrit-logging-cleanup-old-logs ()
  "Remove log files older than retention period."
  (interactive)
  (let* ((log-dir (expand-file-name efrit-logging-directory))
         (cutoff-time (- (float-time)
                         (* efrit-logging-retention-days 24 60 60)))
         (removed-count 0))
    (dolist (file (directory-files-recursively log-dir "\\.jsonl"))
      (when (< (float-time (file-attribute-modification-time 
                             (file-attributes file)))
               cutoff-time)
        (delete-file file)
        (cl-incf removed-count)))
    (when (> removed-count 0)
      (efrit-logging-log 'info 'logging 
                         (format "Removed %d old log files" removed-count)))))

;;; Log viewing and analysis

(defun efrit-logging-view-recent (n)
  "View the N most recent log entries."
  (interactive "nNumber of entries: ")
  (let* ((log-dir (expand-file-name efrit-logging-directory))
         (session-file (efrit-logging--get-log-file 'session))
         (buffer (get-buffer-create "*Efrit Logs*")))
    (with-current-buffer buffer
      (erase-buffer)
      (when (file-exists-p session-file)
        (insert-file-contents session-file)
        (goto-char (point-max))
        (let ((entries nil)
              (count 0))
          (while (and (> (point) (point-min))
                      (< count n))
            (forward-line -1)
            (when (looking-at "^{")
              (push (buffer-substring (line-beginning-position)
                                       (line-end-position))
                    entries)
              (cl-incf count)))
          (erase-buffer)
          (dolist (entry entries)
            (insert (efrit-logging--format-entry entry) "\n\n"))))
      (goto-char (point-min))
      (json-mode))
    (switch-to-buffer buffer)))

(defun efrit-logging--format-entry (json-string)
  "Format a JSON-STRING log entry for display."
  (condition-case nil
      (let* ((json-object-type 'plist)
             (entry (json-read-from-string json-string))
             (type (plist-get entry :type))
             (timestamp (plist-get entry :timestamp))
             (level (plist-get entry :level))
             (message (plist-get entry :message)))
        (format "[%s] %s %s: %s"
                (or timestamp "")
                (or type "")
                (or level "")
                (or message json-string)))
    (error json-string)))

;;; Statistics and reporting

(defun efrit-logging-statistics ()
  "Display logging statistics."
  (interactive)
  (let* ((log-dir (expand-file-name efrit-logging-directory))
         (request-count 0)
         (response-count 0)
         (error-count 0)
         (total-size 0))
    (dolist (file (directory-files-recursively log-dir "\\.jsonl$"))
      (let ((size (file-attribute-size (file-attributes file))))
        (cl-incf total-size size)
        (cond
         ((string-match-p "requests" file)
          (cl-incf request-count (efrit-logging--count-lines file)))
         ((string-match-p "responses" file)
          (cl-incf response-count (efrit-logging--count-lines file)))
         ((string-match-p "errors" file)
          (cl-incf error-count (efrit-logging--count-lines file))))))
    (message "Efrit Logging Statistics:
  Requests: %d
  Responses: %d
  Errors: %d
  Total size: %.2f MB
  Session: %s"
             request-count
             response-count
             error-count
             (/ total-size 1024.0 1024.0)
             (or efrit-logging--session-id "none"))))

(defun efrit-logging--count-lines (file)
  "Count lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (count-lines (point-min) (point-max))))

;;; Initialization

(defun efrit-logging-initialize ()
  "Initialize the logging system."
  (when efrit-logging-enabled
    (efrit-logging--init-session)
    (add-hook 'kill-emacs-hook #'efrit-logging-cleanup)))

(defun efrit-logging-cleanup ()
  "Clean up logging system."
  (when efrit-logging--session-id
    (efrit-logging-log 'info 'logging "Session ended")
    (setq efrit-logging--session-id nil)))

(provide 'efrit-logging)
;;; efrit-logging.el ends here