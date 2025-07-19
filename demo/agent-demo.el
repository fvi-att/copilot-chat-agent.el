;;; agent-demo.el --- Demo script for Copilot Chat Agent functionality -*- lexical-binding: t -*-

;; Demo script to showcase agent functionality without requiring actual Copilot API

;;; Code:

(require 'package)
(package-initialize)

(add-to-list 'load-path ".")
(require 'copilot-chat-agent)

;; Mock instance for demo purposes
(defvar demo-instance nil)

(defun demo-create-mock-instance ()
  "Create a mock instance for demo purposes."
  (setq demo-instance (list :directory default-directory
                           :model "gpt-4o"
                           :buffers nil)))

(defun demo-simulate-response (response)
  "Simulate processing a Copilot response with commands."
  (message "\n=== Simulating Copilot Response ===")
  (message "Response: %s" response)
  (message "\n=== Agent Processing ===")
  
  ;; Extract commands
  (let ((commands (copilot-chat-agent--extract-commands response)))
    (if commands
        (progn
          (message "Found %d commands: %s" (length commands) commands)
          (dolist (command commands)
            (let ((category (copilot-chat-agent--classify-command command))
                  (validation-error (copilot-chat-agent--validate-command command))
                  (should-auto-execute (copilot-chat-agent--should-auto-execute-p command)))
              (message "\nCommand: %s" command)
              (message "  Category: %s" category)
              (message "  Validation: %s" (or validation-error "OK"))
              (message "  Auto-execute: %s" should-auto-execute)
              
              (cond
               (validation-error
                (message "  Status: BLOCKED - %s" validation-error))
               (should-auto-execute
                (message "  Status: AUTO-EXECUTED")
                (message "  Output: [Simulated execution - %s]" command))
               (t
                (message "  Status: REQUIRES USER CONFIRMATION"))))))
      (message "No commands found in response"))))

(defun demo-run-scenarios ()
  "Run demo scenarios for agent functionality."
  (interactive)
  
  (message "=== Copilot Chat Agent Demo ===\n")
  
  ;; Setup
  (demo-create-mock-instance)
  (copilot-chat-agent-mode-enable)
  (message "Agent mode enabled: %s" (copilot-chat-agent-mode-p))
  
  ;; Scenario 1: Read-only commands
  (message "\n--- Scenario 1: Read-only commands (level: read-only) ---")
  (setq copilot-chat-agent-auto-run-level 'read-only)
  (demo-simulate-response "Here's how to check the current directory:\n\n```bash\nls -la\npwd\nwhoami\n```")
  
  ;; Scenario 2: Safe commands
  (message "\n--- Scenario 2: Safe commands (level: safe) ---")
  (setq copilot-chat-agent-auto-run-level 'safe)
  (demo-simulate-response "Let's create a new project structure:\n\n```bash\nmkdir -p project/src\ntouch project/README.md\nls project/\n```")
  
  ;; Scenario 3: Mixed safety levels
  (message "\n--- Scenario 3: Mixed safety levels (level: safe) ---")
  (demo-simulate-response "Let's check and clean up some files:\n\n```bash\nls -la\nrm old-file.txt\nls -la\n```")
  
  ;; Scenario 4: Dangerous commands
  (message "\n--- Scenario 4: Dangerous commands (level: safe) ---")
  (demo-simulate-response "System maintenance commands:\n\n```bash\nsudo systemctl restart nginx\nrm -rf /tmp/*\n```")
  
  ;; Scenario 5: Forbidden commands
  (message "\n--- Scenario 5: Forbidden commands (any level) ---")
  (demo-simulate-response "Dangerous system commands:\n\n```bash\nsudo rm -rf /\ndd if=/dev/zero of=/dev/sda\n```")
  
  ;; Display statistics
  (message "\n--- Agent Statistics ---")
  (copilot-chat-agent-statistics)
  
  (message "\n=== Demo completed ==="))

(defun demo-interactive-test ()
  "Interactive test of agent functionality."
  (interactive)
  
  (message "=== Interactive Agent Test ===")
  (demo-create-mock-instance)
  
  (let ((response (read-string "Enter a response with shell commands: ")))
    (when (not (string-empty-p response))
      (demo-simulate-response response)))
  
  (message "Test completed."))

;; Configuration example
(defun demo-show-configuration ()
  "Show example configuration for agent mode."
  (interactive)
  
  (message "=== Agent Mode Configuration Example ===")
  (message "
;; Basic setup
(require 'copilot-chat-agent)

;; Enable agent mode
(copilot-chat-agent-mode-enable)

;; Set auto-run level (recommended: 'safe or 'read-only)
(setq copilot-chat-agent-auto-run-level 'safe)

;; Optional: Customize working directory
(setq copilot-chat-agent-working-directory \"~/projects/\")

;; Optional: Customize timeout
(setq copilot-chat-agent-timeout 60)

;; Key bindings (optional)
(global-set-key (kbd \"C-c a t\") 'copilot-chat-agent-mode-toggle)
(global-set-key (kbd \"C-c a s\") 'copilot-chat-agent-statistics)
(global-set-key (kbd \"C-c a d\") 'copilot-chat-agent-debug-info)

;; Access via transient menu
(copilot-chat-transient)  ; Press 'A' for Agent menu
"))

;; Run demo if called directly
(when (member "--demo" command-line-args)
  (demo-run-scenarios))

(provide 'agent-demo)
;;; agent-demo.el ends here