;;; efrit-env.el --- Environment configuration for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, environment
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This module provides environment variable configuration for Efrit, including:
;; - Loading from .env files
;; - Direnv integration
;; - Environment variable override system
;; - Secure API key management

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup efrit-env nil
  "Environment configuration for Efrit."
  :group 'efrit
  :prefix "efrit-env-")

(defcustom efrit-env-file ".env"
  "Path to environment file relative to project root."
  :type 'string
  :group 'efrit-env)

(defcustom efrit-env-use-direnv t
  "Whether to use direnv if available."
  :type 'boolean
  :group 'efrit-env)

(defcustom efrit-env-override-existing nil
  "Whether to override existing environment variables."
  :type 'boolean
  :group 'efrit-env)

;;; Environment variable definitions

(defconst efrit-env-variables
  '(;; API Configuration
    ("ANTHROPIC_API_KEY" . efrit-api-key)
    ("EFRIT_API_URL" . efrit-api-url)
    ("EFRIT_API_TIMEOUT" . efrit-api-timeout)
    
    ;; Model Configuration
    ("EFRIT_MODEL" . efrit-model)
    ("EFRIT_MAX_TOKENS" . efrit-max-tokens)
    ("EFRIT_TEMPERATURE" . efrit-temperature)
    
    ;; Multi-turn Configuration
    ("EFRIT_MULTI_TURN_ENABLED" . efrit-multi-turn-enabled)
    ("EFRIT_MULTI_TURN_MAX_TURNS" . efrit-multi-turn-max-turns)
    ("EFRIT_MULTI_TURN_TIMEOUT" . efrit-multi-turn-timeout)
    
    ;; Tool Configuration
    ("EFRIT_ENABLE_TOOLS" . efrit-tools-enabled)
    ("EFRIT_TOOLS_REQUIRE_CONFIRMATION" . efrit-tools-require-confirmation)
    
    ;; Debug Configuration
    ("EFRIT_DEBUG" . efrit-debug)
    ("EFRIT_VERBOSE" . efrit-verbose)
    
    ;; Testing
    ("EFRIT_SKIP_API_TESTS" . efrit-skip-api-tests))
  "Mapping of environment variables to Efrit variables.")

;;; Core functions

(defun efrit-env-find-file ()
  "Find the .env file in the project root."
  (let* ((default-directory (or (and (fboundp 'projectile-project-root)
                                      (projectile-project-root))
                                 (and (fboundp 'project-root)
                                      (project-root (project-current)))
                                 default-directory))
         (env-file (expand-file-name efrit-env-file)))
    (when (file-exists-p env-file)
      env-file)))

(defun efrit-env-parse-file (file)
  "Parse environment FILE and return an alist of variables."
  (let ((env-vars nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\([A-Z_]+\\)=\\(.*\\)$")
          (let ((var (match-string 1))
                (val (match-string 2)))
            ;; Remove quotes if present
            (when (string-match "^['\"]\\(.*\\)['\"]$" val)
              (setq val (match-string 1 val)))
            (push (cons var val) env-vars)))
        (forward-line)))
    (nreverse env-vars)))

(defun efrit-env-load-file (file)
  "Load environment variables from FILE."
  (interactive "fEnvironment file: ")
  (if (file-exists-p file)
      (let ((env-vars (efrit-env-parse-file file))
            (loaded 0))
        (dolist (pair env-vars)
          (let* ((env-var (car pair))
                 (env-val (cdr pair))
                 (elisp-var (cdr (assoc env-var efrit-env-variables))))
            (when (and elisp-var
                       (or efrit-env-override-existing
                           (not (getenv env-var))))
              (setenv env-var env-val)
              (when (boundp elisp-var)
                (set elisp-var (efrit-env-parse-value env-val)))
              (cl-incf loaded))))
        (message "Loaded %d environment variables from %s" loaded file))
    (message "Environment file not found: %s" file)))

(defun efrit-env-parse-value (value)
  "Parse VALUE from string to appropriate Elisp type."
  (cond
   ;; Boolean values
   ((string-match-p "^\\(t\\|true\\|yes\\|1\\)$" (downcase value)) t)
   ((string-match-p "^\\(nil\\|false\\|no\\|0\\)$" (downcase value)) nil)
   ;; Numeric values
   ((string-match-p "^[0-9]+$" value) (string-to-number value))
   ((string-match-p "^[0-9]*\\.[0-9]+$" value) (string-to-number value))
   ;; String values
   (t value)))

;;; Direnv integration

(defun efrit-env-use-direnv ()
  "Load environment using direnv if available."
  (when (and efrit-env-use-direnv
             (executable-find "direnv"))
    (let* ((default-directory (or (and (fboundp 'projectile-project-root)
                                        (projectile-project-root))
                                   default-directory))
           (output (shell-command-to-string "direnv export elisp")))
      (when (string-match "^(" output)
        (eval (read output))
        (message "Loaded environment from direnv")
        t))))

;;; Auto-loading

(defun efrit-env-auto-load ()
  "Automatically load environment configuration."
  (or (efrit-env-use-direnv)
      (when-let ((env-file (efrit-env-find-file)))
        (efrit-env-load-file env-file))))

;;; Interactive commands

;;;###autoload
(defun efrit-env-load ()
  "Load Efrit environment configuration."
  (interactive)
  (efrit-env-auto-load))

;;;###autoload
(defun efrit-env-show ()
  "Show current Efrit environment variables."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Environment*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Environment Variables\n")
      (insert "===========================\n\n")
      (dolist (pair efrit-env-variables)
        (let* ((env-var (car pair))
               (elisp-var (cdr pair))
               (env-val (getenv env-var))
               (elisp-val (and (boundp elisp-var) (symbol-value elisp-var))))
          (insert (format "%-40s = %s\n" env-var (or env-val "(not set)")))
          (when elisp-val
            (insert (format "  -> %s = %S\n" elisp-var elisp-val)))
          (insert "\n"))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun efrit-env-create-template ()
  "Create a .env.example template file."
  (interactive)
  (let ((template-file (expand-file-name ".env.example")))
    (with-temp-file template-file
      (insert "# Efrit Environment Configuration\n")
      (insert "# Copy this file to .env and uncomment/modify as needed\n\n")
      (insert "# API Configuration\n")
      (insert "# ANTHROPIC_API_KEY=sk-ant-api03-...\n")
      (insert "# EFRIT_API_URL=https://api.anthropic.com/v1/messages\n\n")
      (insert "# Model Configuration\n")
      (insert "# EFRIT_MODEL=claude-3-5-sonnet-20241022\n")
      (insert "# EFRIT_MAX_TOKENS=8192\n")
      (insert "# EFRIT_TEMPERATURE=0.1\n\n")
      (insert "# Multi-turn Configuration\n")
      (insert "# EFRIT_MULTI_TURN_ENABLED=t\n")
      (insert "# EFRIT_MULTI_TURN_MAX_TURNS=10\n\n")
      (insert "# Debug Configuration\n")
      (insert "# EFRIT_DEBUG=nil\n")
      (insert "# EFRIT_VERBOSE=nil\n\n")
      (insert "# Testing\n")
      (insert "# EFRIT_SKIP_API_TESTS=nil\n"))
    (message "Created template: %s" template-file)))

;;; Makefile support

;;;###autoload
(defun efrit-env-generate-makefile-target ()
  "Generate Makefile target for .env file creation."
  (interactive)
  (let ((makefile-content
         "# Environment configuration
.env: .env.example
\t@echo \"Creating .env from .env.example...\"
\t@cp .env.example .env
\t@echo \"✅ Created .env file\"
\t@echo \"Please edit .env and add your API key\"

.PHONY: env
env: .env
\t@echo \"Environment configured\""))
    (if (called-interactively-p 'interactive)
        (progn
          (switch-to-buffer (get-buffer-create "*Makefile snippet*"))
          (erase-buffer)
          (insert makefile-content)
          (makefile-mode))
      makefile-content)))

;;; Initialize on load

(defun efrit-env-initialize ()
  "Initialize environment configuration."
  (efrit-env-auto-load))

;; Load environment when this file is loaded
(eval-after-load 'efrit-env
  '(efrit-env-initialize))

(provide 'efrit-env)
;;; efrit-env.el ends here