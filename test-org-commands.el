;;; test-org-commands.el --- Test org-mode command extraction -*- lexical-binding: t -*-

(require 'package)
(package-initialize)

(add-to-list 'load-path ".")
(require 'copilot-chat-agent)

(defun test-org-command-extraction ()
  "Test command extraction from org-mode formatted text."
  (let ((org-response "*** このマシンのグローバルIPアドレスを調べる方法

以下のコマンドを実行することで、このマシンのグローバルIPアドレスを確認できます。

#+BEGIN_SRC shell
curl ifconfig.me
#+END_SRC

または、以下のコマンドでも同様の結果を得ることができます。

#+BEGIN_SRC shell
curl -s https://api.ipify.org
#+END_SRC

これらのコマンドをターミナルで実行すると、グローバルIPアドレスが表示されます。"))
    
    (message "=== Testing Org-mode Command Extraction ===")
    (message "Input text:\n%s" org-response)
    
    (let ((commands (copilot-chat-agent--extract-commands org-response)))
      (message "\nExtracted commands: %s" commands)
      (message "Number of commands: %d" (length commands))
      
      (if (and (member "curl ifconfig.me" commands)
               (member "curl -s https://api.ipify.org" commands))
          (message "✓ SUCCESS: Org-mode command extraction working")
        (message "✗ FAILED: Org-mode command extraction not working"))
      
      ;; Test classification
      (dolist (command commands)
        (let ((category (copilot-chat-agent--classify-command command)))
          (message "Command: %s -> Category: %s" command category))))))

(defun test-agent-integration ()
  "Test full agent integration."
  (message "\n=== Testing Agent Integration ===")
  
  ;; Enable agent mode
  (copilot-chat-agent-mode-enable)
  (setq copilot-chat-agent-auto-run-level 'read-only)
  
  (message "Agent mode enabled: %s" (copilot-chat-agent-mode-p))
  (message "Auto-run level: %s" copilot-chat-agent-auto-run-level)
  
  ;; Create mock instance
  (let ((mock-instance (list :directory default-directory
                            :chat-buffer (current-buffer))))
    
    ;; Test command processing without actual execution (non-interactive)
    (let ((org-response "#+BEGIN_SRC shell\ncurl ifconfig.me\n#+END_SRC"))
      (message "Processing response: %s" org-response)
      
      ;; Test command extraction and classification only
      (let ((commands (copilot-chat-agent--extract-commands org-response)))
        (message "Found commands: %s" commands)
        (dolist (command commands)
          (let ((category (copilot-chat-agent--classify-command command))
                (should-auto (copilot-chat-agent--should-auto-execute-p command)))
            (message "Command: %s, Category: %s, Auto-execute: %s" 
                    command category should-auto)))))))

(defun run-org-tests ()
  "Run all org-mode related tests."
  (test-org-command-extraction)
  (test-agent-integration)
  (message "\n=== All tests completed ==="))

;; Run tests if called directly
(when (member "--test-org" command-line-args)
  (run-org-tests))

(provide 'test-org-commands)
;;; test-org-commands.el ends here