;;; test-agent.el --- Test script for copilot-chat-agent -*- lexical-binding: t -*-

;; Test script for Copilot Chat Agent functionality

;;; Code:

(require 'package)
(package-initialize)

(add-to-list 'load-path ".")
(require 'copilot-chat-agent)

;; Test command classification
(defun test-command-classification ()
  "Test command classification functionality."
  (let ((test-cases
         '(("ls -la" . read-only)
           ("cat file.txt" . read-only)
           ("mkdir test" . safe)
           ("touch file.txt" . safe)
           ("mv file1 file2" . write)
           ("rm file.txt" . dangerous)
           ("sudo rm -rf /" . forbidden)
           ("unknown-command" . unknown))))
    
    (message "=== Command Classification Tests ===")
    (dolist (test test-cases)
      (let ((cmd (car test))
            (expected (cdr test))
            (actual (copilot-chat-agent--classify-command (car test))))
        (if (eq actual expected)
            (message "✓ PASS: %s -> %s" cmd actual)
          (message "✗ FAIL: %s -> %s (expected %s)" cmd actual expected))))))

;; Test command extraction
(defun test-command-extraction ()
  "Test command extraction from responses."
  (let ((response "Here are some commands to run:

```bash
ls -la
cat README.md
```

And here's some Python code:

```python
print(\"hello\")
```

And another shell command:

```sh
mkdir test-dir
touch test-file.txt
```"))
    
    (message "\n=== Command Extraction Tests ===")
    (let ((commands (copilot-chat-agent--extract-commands response)))
      (message "Extracted commands: %s" commands)
      (if (and (member "ls -la" commands)
               (member "cat README.md" commands)
               (member "mkdir test-dir" commands)
               (member "touch test-file.txt" commands)
               (not (member "print(\"hello\")" commands)))
          (message "✓ PASS: Command extraction working correctly")
        (message "✗ FAIL: Command extraction not working as expected")))))

;; Test auto-execution decision
(defun test-auto-execution ()
  "Test auto-execution decision logic."
  (message "\n=== Auto-Execution Tests ===")
  
  ;; Test different levels
  (let ((test-cases
         '((none "ls -la" nil)
           (read-only "ls -la" t)
           (read-only "mkdir test" nil)
           (safe "ls -la" t)
           (safe "mkdir test" t)
           (safe "rm file" nil)
           (write "mv file1 file2" t)
           (all "sudo command" t)
           (all "rm -rf /" nil)))) ; forbidden should never auto-execute
    
    (dolist (test test-cases)
      (let ((level (nth 0 test))
            (command (nth 1 test))
            (expected (nth 2 test)))
        (let ((copilot-chat-agent-auto-run-level level))
          (let ((actual (copilot-chat-agent--should-auto-execute-p command)))
            (if (eq (if actual t nil) expected)
                (message "✓ PASS: Level %s, command '%s' -> %s" level command actual)
              (message "✗ FAIL: Level %s, command '%s' -> %s (expected %s)" 
                      level command actual expected))))))))

;; Test validation
(defun test-command-validation ()
  "Test command validation functionality."
  (message "\n=== Command Validation Tests ===")
  
  (let ((test-cases
         '(("ls -la" . nil)
           ("mkdir test" . nil)
           ("cd ../" . "Path traversal detected")
           ("rm -rf /" . "Command is forbidden for security reasons")
           ("" . "Empty command"))))
    
    (dolist (test test-cases)
      (let ((command (car test))
            (expected-error (cdr test))
            (actual-error (copilot-chat-agent--validate-command command)))
        (if (equal actual-error expected-error)
            (message "✓ PASS: Validation for '%s'" command)
          (message "✗ FAIL: Validation for '%s' -> %s (expected %s)" 
                  command actual-error expected-error))))))

;; Run all tests
(defun run-agent-tests ()
  "Run all agent tests."
  (message "Starting Copilot Chat Agent Tests...\n")
  
  (test-command-classification)
  (test-command-extraction)
  (test-auto-execution)
  (test-command-validation)
  
  (message "\n=== Agent Mode Tests ===")
  (message "Agent mode enabled: %s" (copilot-chat-agent-mode-p))
  (copilot-chat-agent-mode-enable)
  (message "After enable: %s" (copilot-chat-agent-mode-p))
  (copilot-chat-agent-mode-disable)
  (message "After disable: %s" (copilot-chat-agent-mode-p))
  
  (message "\nAll tests completed!"))

;; Run tests if called directly
(when (member "--run-tests" command-line-args)
  (run-agent-tests))

(provide 'test-agent)
;;; test-agent.el ends here