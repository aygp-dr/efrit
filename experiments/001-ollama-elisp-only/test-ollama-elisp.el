;;; test-ollama-elisp.el --- Test cases for ollama-elisp-only -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite demonstrating various use cases for the Ollama Elisp-only interface.
;; These tests show how the system responds to different types of queries.

;;; Code:

(require 'ert)
(add-to-list 'load-path ".")
(require 'ollama-elisp-only)

;; Example prompts that should generate executable Elisp
(defvar test-ollama-elisp-prompts
  '(("list files in current directory"
     . "(directory-files default-directory)")
    
    ("what is 2 + 2"
     . "(+ 2 2)")
    
    ("get emacs version"
     . "emacs-version")
    
    ("show current time"
     . "(current-time-string)")
    
    ("count lines in current buffer"
     . "(count-lines (point-min) (point-max))")
    
    ("get first 5 files in home directory"
     . "(seq-take (directory-files \"~\" nil \"^[^.]\") 5)")
    
    ("check if file exists: /tmp/test.txt"
     . "(file-exists-p \"/tmp/test.txt\")")
    
    ("create a list of numbers from 1 to 10"
     . "(number-sequence 1 10)")
    
    ("define a function that doubles a number"
     . "(defun double (n) (* n 2))")
    
    ("filter even numbers from 1 to 20"
     . "(seq-filter (lambda (n) (= (mod n 2) 0)) (number-sequence 1 20))"))
  "Example prompts and their expected Elisp-like responses.")

(defun test-ollama-elisp-manual ()
  "Manual test function to verify Ollama responses.
Run this interactively to test actual Ollama integration."
  (interactive)
  (if (not (y-or-n-p "This will make actual API calls to Ollama. Continue? "))
      (message "Test cancelled")
    (dolist (test test-ollama-elisp-prompts)
      (let* ((prompt (car test))
             (expected-pattern (cdr test)))
        (message "\n--- Testing prompt: %s" prompt)
        (message "Expected pattern: %s" expected-pattern)
        (condition-case err
            (let ((result (ollama-elisp-query prompt)))
              (message "Result: %S" result))
          (error (message "Error: %s" err)))
        (sit-for 1)))))

;; Mock function for testing without Ollama
(defun ollama-elisp--mock-request (prompt)
  "Mock request function for PROMPT that returns predetermined Elisp."
  (cond
   ((string-match-p "list.*files" prompt)
    "{\"elisp\": \"(directory-files default-directory)\"}")
   ((string-match-p "current.*directory" prompt)
    "{\"elisp\": \"default-directory\"}")
   ((string-match-p "2.*2" prompt)
    "{\"elisp\": \"(+ 2 2)\"}")
   ((string-match-p "time" prompt)
    "{\"elisp\": \"(current-time-string)\"}")
   (t
    "{\"elisp\": \"(message \\\"Unknown request\\\")\"}")))

(ert-deftest test-ollama-elisp-extract-code ()
  "Test code extraction from various response formats."
  (should (equal (ollama-elisp--extract-code "{\"elisp\": \"(+ 1 2)\"}")
                 "(+ 1 2)"))
  (should (equal (ollama-elisp--extract-code "(+ 3 4)")
                 "(+ 3 4)")))

(ert-deftest test-ollama-elisp-basic-math ()
  "Test that basic math expressions can be evaluated."
  (let ((code "(+ 2 2)"))
    (should (= (eval (read code)) 4))))

(ert-deftest test-ollama-elisp-file-operations ()
  "Test file operation expressions."
  (let ((code "(file-exists-p \"/tmp\")"))
    (should (booleanp (eval (read code))))))

(ert-deftest test-ollama-elisp-list-operations ()
  "Test list operation expressions."
  (let ((code "(length '(a b c d))"))
    (should (= (eval (read code)) 4))))

;; Interactive demonstration function
(defun ollama-elisp-demo ()
  "Interactive demonstration of Ollama Elisp-only mode."
  (interactive)
  (let ((demos
         '(("Show current directory" . "what is the current directory")
           ("List elisp files" . "list all .el files in current directory")
           ("Buffer info" . "show current buffer name and size")
           ("Math calculation" . "calculate factorial of 5")
           ("Data structure" . "create an alist with three key-value pairs")
           ("String manipulation" . "reverse the string 'hello world'"))))
    (dolist (demo demos)
      (when (y-or-n-p (format "Demo: %s? " (car demo)))
        (let ((result (ollama-elisp-query (cdr demo))))
          (message "Result: %S" result)
          (sit-for 2))))))

;; Performance test
(defun ollama-elisp-benchmark ()
  "Benchmark Ollama response times."
  (interactive)
  (let ((prompts '("(+ 1 1)"
                   "list files"
                   "current time"
                   "emacs version"))
        (times '()))
    (dolist (prompt prompts)
      (let ((start (current-time)))
        (condition-case nil
            (ollama-elisp-query prompt)
          (error nil))
        (push (float-time (time-subtract (current-time) start)) times)))
    (message "Response times: %s\nAverage: %.3f seconds"
             (reverse times)
             (/ (apply #'+ times) (length times)))))

(provide 'test-ollama-elisp)
;;; test-ollama-elisp.el ends here