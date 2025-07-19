;;; test-chat-output.el --- Test chat buffer output functionality -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(add-to-list 'load-path ".")
(require 'copilot-chat-agent)

(defun test-chat-buffer-output ()
  "Test chat buffer output formatting."
  (interactive)
  
  (message "=== Testing Chat Buffer Output ===")
  
  ;; Test different frontend formats
  (let ((test-cases
         '((org "curl ifconfig.me" 0 "192.168.1.100")
           (markdown "ls -la" 0 "total 8\ndrwxr-xr-x  4 user staff 128 Jul 19 16:00 .")
           (shell-maker "pwd" 0 "/Users/test")
           (org "false" 1 "")
           (markdown "nonexistent-command" 127 "command not found"))))
    
    (dolist (test test-cases)
      (let ((frontend (nth 0 test))
            (command (nth 1 test))
            (exit-code (nth 2 test))
            (output (nth 3 test)))
        
        (message "\n--- Testing %s frontend ---" frontend)
        (message "Command: %s" command)
        (message "Exit code: %d" exit-code)
        (message "Output: %s" (if (string-empty-p output) "(empty)" output))
        
        (let ((formatted (copilot-chat-agent--format-execution-result 
                         frontend command exit-code output)))
          (message "Formatted result:\n%s" formatted)))))
  
  (message "\n=== Test completed ==="))

;; Run test if called directly
(when (member "--test-output" command-line-args)
  (test-chat-buffer-output))

(provide 'test-chat-output)
;;; test-chat-output.el ends here