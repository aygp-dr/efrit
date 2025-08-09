;;; efrit-demo.el --- Demo and example system for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, demo
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This module provides a demo system for Efrit, including:
;; - Predefined example commands
;; - Recording capabilities with asciinema
;; - Interactive demo mode
;; - Example generation

;;; Code:

(require 'efrit-do)

;;; Customization

(defgroup efrit-demo nil
  "Demo and example system for Efrit."
  :group 'efrit
  :prefix "efrit-demo-")

(defcustom efrit-demo-examples
  '(;; Creative content
    ("haiku" . "Write a haiku about debugging code at midnight")
    ("ascii-art" . "Draw ASCII art of a robot")
    
    ;; Code generation
    ("highlight-todo" . "Write a function that highlights all TODO and FIXME comments in rainbow colors")
    ("random-quote" . "Create an interactive function that shows a random motivational quote")
    ("number-game" . "Create a simple number guessing game that runs in the minibuffer")
    
    ;; Buffer operations
    ("multi-buffer" . "Create three buffers: notes.org with meeting agenda, tasks.org with weekly TODOs, and tips.txt with Emacs shortcuts")
    ("markdown-table" . "Generate a markdown table with 5 programming languages and their best use cases")
    ("org-planner" . "Create an org-mode weekly planner with time blocks from 9am to 5pm")
    
    ;; Context-aware
    ("make-fun" . "Add emoji and make the previous content more fun")
    ("explain-code" . "Explain what the last function does in simple terms"))
  "List of demo examples as (key . command) pairs."
  :type '(alist :key-type string :value-type string)
  :group 'efrit-demo)

(defcustom efrit-demo-delay 2
  "Delay in seconds between demo commands."
  :type 'number
  :group 'efrit-demo)

(defcustom efrit-demo-typing-speed 0.05
  "Simulated typing speed in seconds per character."
  :type 'number
  :group 'efrit-demo)

;;; Demo execution

(defvar efrit-demo--current-examples nil
  "Current list of examples being demonstrated.")

(defvar efrit-demo--timer nil
  "Timer for running demo commands.")

;;;###autoload
(defun efrit-demo-run-example (key)
  "Run a specific demo example by KEY."
  (interactive
   (list (completing-read "Demo example: "
                          efrit-demo-examples
                          nil t)))
  (if-let ((command (alist-get key efrit-demo-examples nil nil #'string=)))
      (progn
        (message "Running demo: %s" key)
        (efrit-do command))
    (user-error "Unknown demo example: %s" key)))

;;;###autoload
(defun efrit-demo-run-all ()
  "Run all demo examples sequentially."
  (interactive)
  (setq efrit-demo--current-examples (mapcar #'cdr efrit-demo-examples))
  (efrit-demo--run-next))

(defun efrit-demo--run-next ()
  "Run the next demo example."
  (if efrit-demo--current-examples
      (let ((command (pop efrit-demo--current-examples)))
        (message "Demo: %s" command)
        (efrit-do command)
        (setq efrit-demo--timer
              (run-with-timer efrit-demo-delay nil #'efrit-demo--run-next)))
    (message "Demo complete!")))

;;;###autoload
(defun efrit-demo-stop ()
  "Stop the running demo."
  (interactive)
  (when efrit-demo--timer
    (cancel-timer efrit-demo--timer)
    (setq efrit-demo--timer nil))
  (setq efrit-demo--current-examples nil)
  (message "Demo stopped"))

;;; Recording functions

;;;###autoload
(defun efrit-demo-record-asciinema ()
  "Start recording a demo with asciinema."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (filename (expand-file-name 
                    (format "demos/efrit-demo-%s.cast" timestamp)
                    (file-name-directory (locate-library "efrit"))))
         (title "Efrit: AI-Powered Emacs Assistant Demo"))
    (make-directory (file-name-directory filename) t)
    (start-process "asciinema-record" "*asciinema*"
                   "asciinema" "rec"
                   "--title" title
                   "--idle-time-limit" "3"
                   filename)
    (message "Recording started: %s" filename)))

;;;###autoload
(defun efrit-demo-convert-to-gif (cast-file)
  "Convert CAST-FILE to GIF using agg."
  (interactive "fCast file: ")
  (let ((gif-file (concat (file-name-sans-extension cast-file) ".gif")))
    (if (executable-find "agg")
        (progn
          (start-process "agg-convert" "*agg*"
                         "agg" cast-file gif-file)
          (message "Converting to GIF: %s" gif-file))
      (user-error "agg not found. Install with: pip install asciinema-gif-generator"))))

;;; Interactive demo mode

(defvar efrit-demo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'efrit-demo-next)
    (define-key map (kbd "p") #'efrit-demo-previous)
    (define-key map (kbd "r") #'efrit-demo-run-current)
    (define-key map (kbd "q") #'efrit-demo-quit)
    (define-key map (kbd "?") #'efrit-demo-help)
    map)
  "Keymap for efrit-demo-mode.")

(define-derived-mode efrit-demo-mode special-mode "Efrit-Demo"
  "Major mode for Efrit demos.
\\{efrit-demo-mode-map}"
  (setq-local efrit-demo--current-index 0))

(defvar efrit-demo--current-index 0
  "Current demo example index.")

;;;###autoload
(defun efrit-demo-interactive ()
  "Start an interactive demo session."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Demo*")))
    (with-current-buffer buffer
      (efrit-demo-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Efrit Interactive Demo\n")
        (insert "======================\n\n")
        (insert "Commands:\n")
        (insert "  n - Next example\n")
        (insert "  p - Previous example\n")
        (insert "  r - Run current example\n")
        (insert "  q - Quit demo\n")
        (insert "  ? - Show this help\n\n")
        (insert "Examples:\n")
        (insert "----------\n")
        (let ((index 0))
          (dolist (example efrit-demo-examples)
            (insert (format "%2d. [%s] %s\n" 
                            (1+ index)
                            (car example)
                            (cdr example)))
            (setq index (1+ index))))
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

(defun efrit-demo-next ()
  "Go to next demo example."
  (interactive)
  (setq efrit-demo--current-index 
        (mod (1+ efrit-demo--current-index) 
             (length efrit-demo-examples)))
  (efrit-demo--highlight-current))

(defun efrit-demo-previous ()
  "Go to previous demo example."
  (interactive)
  (setq efrit-demo--current-index 
        (mod (1- efrit-demo--current-index) 
             (length efrit-demo-examples)))
  (efrit-demo--highlight-current))

(defun efrit-demo-run-current ()
  "Run the current demo example."
  (interactive)
  (let ((example (nth efrit-demo--current-index efrit-demo-examples)))
    (efrit-do (cdr example))))

(defun efrit-demo-quit ()
  "Quit the demo."
  (interactive)
  (quit-window))

(defun efrit-demo-help ()
  "Show demo help."
  (interactive)
  (message "n:next p:prev r:run q:quit ?:help"))

(defun efrit-demo--highlight-current ()
  "Highlight the current demo example."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "Examples:" nil t)
      (forward-line 2)
      (forward-line efrit-demo--current-index)
      (let ((inhibit-read-only t))
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'face 'highlight)))))

(provide 'efrit-demo)
;;; efrit-demo.el ends here