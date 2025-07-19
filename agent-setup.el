;;; agent-setup.el --- Setup script for Copilot Chat Agent integration -*- lexical-binding: t -*-

;; Setup and test script for the agent functionality

;;; Code:

(require 'package)
(package-initialize)

;; Load copilot-chat and agent modules
(add-to-list 'load-path ".")
(require 'copilot-chat)
(require 'copilot-chat-agent)

;; Enable agent mode
(copilot-chat-agent-mode-enable)

;; Set auto-run level to safe (allows curl commands)
(setq copilot-chat-agent-auto-run-level 'read-only)

;; Display status
(message "=== Copilot Chat Agent Setup Complete ===")
(message "Agent mode: %s" (if copilot-chat-agent-mode "enabled" "disabled"))
(message "Auto-run level: %s" copilot-chat-agent-auto-run-level)
(message "Ready to test!")

;; Test function to verify integration
(defun test-agent-with-sample-response ()
  "Test agent with a sample response."
  (interactive)
  (let ((sample-response "Here's how to check your IP address:

#+BEGIN_SRC shell
curl ifconfig.me
#+END_SRC

This command will show your public IP address."))
    
    (message "\n=== Testing Agent with Sample Response ===")
    (message "Sample response: %s" sample-response)
    
    ;; Extract commands
    (let ((commands (copilot-chat-agent--extract-commands sample-response)))
      (message "Extracted commands: %s" commands)
      
      ;; Test classification
      (dolist (command commands)
        (let ((category (copilot-chat-agent--classify-command command))
              (should-auto (copilot-chat-agent--should-auto-execute-p command)))
          (message "Command: %s" command)
          (message "  Category: %s" category)
          (message "  Will auto-execute: %s" should-auto)
          
          ;; Show what would happen
          (if should-auto
              (message "  → Would execute automatically")
            (message "  → Would ask for user confirmation")))))))

;; Provide instructions
(message "\n=== Usage Instructions ===")
(message "1. Start copilot-chat: M-x copilot-chat-display")
(message "2. Ask Copilot to suggest shell commands")
(message "3. Commands in #+BEGIN_SRC shell blocks will be detected")
(message "4. Safe commands (like 'curl ifconfig.me') will execute automatically")
(message "5. Use M-x copilot-chat-transient and press 'A' for agent settings")
(message "\n=== Test Command ===")
(message "Run: M-x test-agent-with-sample-response")

(provide 'agent-setup)
;;; agent-setup.el ends here