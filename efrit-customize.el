;;; efrit-customize.el --- Customization interface for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, customize
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This module provides a comprehensive customization interface for Efrit
;; using Emacs' built-in customize system. All Efrit settings can be
;; configured through M-x customize-group RET efrit RET

;;; Code:

;;; Main group definition

(defgroup efrit nil
  "AI-powered Emacs coding assistant using Claude."
  :group 'applications
  :prefix "efrit-"
  :link '(url-link :tag "GitHub" "https://github.com/steveyegge/efrit")
  :link '(emacs-commentary-link :tag "Commentary" "efrit.el"))

;;; API Configuration

(defgroup efrit-api nil
  "API configuration for Efrit."
  :group 'efrit
  :prefix "efrit-")

(defcustom efrit-api-key nil
  "Anthropic API key for Claude.
It's recommended to use auth-source (.authinfo) instead of setting this directly."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API Key"))
  :group 'efrit-api)

(defcustom efrit-api-url "https://api.anthropic.com/v1/messages"
  "API endpoint URL for Claude."
  :type 'string
  :group 'efrit-api)

(defcustom efrit-api-timeout 30
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'efrit-api)

;;; Model Configuration

(defgroup efrit-models nil
  "Model selection for Efrit."
  :group 'efrit
  :prefix "efrit-")

(defcustom efrit-model "claude-3-5-sonnet-20241022"
  "Primary Claude model for chat interactions."
  :type '(choice (const :tag "Claude 3.5 Sonnet" "claude-3-5-sonnet-20241022")
                 (const :tag "Claude 3.5 Haiku" "claude-3-5-haiku-20241022")
                 (const :tag "Claude 3 Opus" "claude-3-opus-20240229")
                 (string :tag "Custom model"))
  :group 'efrit-models)

(defcustom efrit-max-tokens 8192
  "Maximum tokens for model responses."
  :type 'integer
  :group 'efrit-models)

(defcustom efrit-temperature 0.1
  "Temperature for model responses (0.0 to 1.0)."
  :type 'number
  :group 'efrit-models)

;;; Multi-turn Configuration

(defgroup efrit-multi-turn nil
  "Multi-turn conversation settings."
  :group 'efrit
  :prefix "efrit-multi-turn-")

(defcustom efrit-multi-turn-enabled t
  "Enable multi-turn conversations."
  :type 'boolean
  :group 'efrit-multi-turn)

(defcustom efrit-multi-turn-max-turns 10
  "Maximum number of turns in a conversation."
  :type 'integer
  :group 'efrit-multi-turn)

(defcustom efrit-multi-turn-timeout 300
  "Timeout in seconds for multi-turn conversations."
  :type 'integer
  :group 'efrit-multi-turn)

;;; Tool Configuration

(defgroup efrit-tools nil
  "Tool and function calling configuration."
  :group 'efrit
  :prefix "efrit-tools-")

(defcustom efrit-tools-enabled t
  "Enable tool/function calling."
  :type 'boolean
  :group 'efrit-tools)

(defcustom efrit-tools-require-confirmation t
  "Require confirmation for destructive operations."
  :type 'boolean
  :group 'efrit-tools)

(defcustom efrit-tools-sexp-evaluation-enabled t
  "Enable S-expression evaluation."
  :type 'boolean
  :group 'efrit-tools)

;;; efrit-do Configuration

(defgroup efrit-do nil
  "Natural language command execution settings."
  :group 'efrit
  :prefix "efrit-do-")

(defcustom efrit-do-buffer-name "*efrit-do*"
  "Name of the efrit-do results buffer."
  :type 'string
  :group 'efrit-do)

(defcustom efrit-do-show-results t
  "Show command results in a buffer."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-history-max 50
  "Maximum number of commands to keep in history."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-debug nil
  "Enable debug output for efrit-do."
  :type 'boolean
  :group 'efrit-do)

;;; UI Configuration

(defgroup efrit-ui nil
  "User interface configuration."
  :group 'efrit
  :prefix "efrit-")

(defcustom efrit-buffer-name "*efrit*"
  "Name of main Efrit buffer."
  :type 'string
  :group 'efrit-ui)

(defcustom efrit-ui-show-timestamps t
  "Show timestamps in conversation."
  :type 'boolean
  :group 'efrit-ui)

(defcustom efrit-ui-show-token-count nil
  "Show token count in responses."
  :type 'boolean
  :group 'efrit-ui)

;;; Development Configuration

(defgroup efrit-development nil
  "Development and debugging settings."
  :group 'efrit
  :prefix "efrit-")

(defcustom efrit-debug nil
  "Enable global debug mode."
  :type 'boolean
  :group 'efrit-development)

(defcustom efrit-verbose nil
  "Enable verbose output."
  :type 'boolean
  :group 'efrit-development)

;;; Convenience functions

;;;###autoload
(defun efrit-customize ()
  "Open Efrit customization interface."
  (interactive)
  (customize-group 'efrit))

;;;###autoload
(defun efrit-customize-save-all ()
  "Save all Efrit customizations."
  (interactive)
  (customize-save-customized)
  (message "Efrit customizations saved"))

;;;###autoload
(defun efrit-customize-reset-to-defaults ()
  "Reset all Efrit settings to defaults."
  (interactive)
  (when (y-or-n-p "Reset all Efrit settings to defaults? ")
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                          (string-prefix-p "efrit-" (symbol-name symbol))
                          (custom-variable-p symbol))
                  (set symbol (eval (car (get symbol 'standard-value)))))))
    (message "Efrit settings reset to defaults")))

;;; Custom themes support

(defun efrit-customize-export-theme (file)
  "Export current Efrit settings to FILE as a theme."
  (interactive "FExport Efrit settings to file: ")
  (let ((settings nil))
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                          (string-prefix-p "efrit-" (symbol-name symbol))
                          (custom-variable-p symbol)
                          (not (equal (symbol-value symbol)
                                      (eval (car (get symbol 'standard-value))))))
                  (push (cons symbol (symbol-value symbol)) settings))))
    (with-temp-file file
      (insert ";;; Efrit custom theme\n")
      (insert ";; Generated: " (current-time-string) "\n\n")
      (insert "(defun efrit-apply-custom-theme ()\n")
      (insert "  \"Apply custom Efrit theme.\"\n")
      (dolist (setting settings)
        (insert (format "  (setq %s %S)\n" (car setting) (cdr setting))))
      (insert "  (message \"Efrit custom theme applied\"))\n\n")
      (insert "(provide 'efrit-custom-theme)\n"))
    (message "Efrit settings exported to %s" file)))

(defun efrit-customize-import-theme (file)
  "Import Efrit settings from FILE."
  (interactive "fImport Efrit settings from file: ")
  (load-file file)
  (when (fboundp 'efrit-apply-custom-theme)
    (efrit-apply-custom-theme)))

;;; Menu bar integration

(easy-menu-define efrit-customize-menu nil
  "Menu for Efrit customization."
  '("Efrit Settings"
    ["Customize All..." efrit-customize t]
    "---"
    ("Groups"
     ["API Configuration..." (customize-group 'efrit-api) t]
     ["Models..." (customize-group 'efrit-models) t]
     ["Multi-turn..." (customize-group 'efrit-multi-turn) t]
     ["Tools..." (customize-group 'efrit-tools) t]
     ["efrit-do..." (customize-group 'efrit-do) t]
     ["UI..." (customize-group 'efrit-ui) t]
     ["Development..." (customize-group 'efrit-development) t])
    "---"
    ["Save All Settings" efrit-customize-save-all t]
    ["Reset to Defaults" efrit-customize-reset-to-defaults t]
    "---"
    ["Export Theme..." efrit-customize-export-theme t]
    ["Import Theme..." efrit-customize-import-theme t]))

;; Add menu to menu bar when loaded
(when (boundp 'menu-bar-mode)
  (easy-menu-add-item menu-bar-tools-menu nil efrit-customize-menu "Games"))

(provide 'efrit-customize)
;;; efrit-customize.el ends here