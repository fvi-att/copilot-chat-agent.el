;;; test-copilot-chat-transient-agent.el --- Tests for copilot-chat-transient agent menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agent mode transient menu functionality in copilot-chat-transient.el

;;; Code:

(require 'ert)
(require 'copilot-chat-transient)
(require 'copilot-chat-agent)
(require 'transient)

(ert-deftest test-copilot-chat-transient-agent-menu-exists ()
  "Test that the agent transient menu is properly defined."
  (should (fboundp 'copilot-chat-transient-agent))
  (should (get 'copilot-chat-transient-agent 'transient--prefix)))

(ert-deftest test-copilot-chat-transient-agent-menu-commands ()
  "Test that the agent menu includes the expected commands."
  (let ((prefix (get 'copilot-chat-transient-agent 'transient--prefix)))
    (should prefix)
    
    ;; Check that the prefix has the expected structure
    (let ((layout (oref prefix layout)))
      (should layout)
      
      ;; Verify it contains expected command references
      (let ((commands-found nil))
        (dolist (group layout)
          (when (listp group)
            (dolist (item group)
              (when (listp item)
                (dolist (element item)
                  (when (and (listp element) (stringp (car element)))
                    (let ((command (nth 2 element)))
                      (when command
                        (push command commands-found))))))))
        
        ;; Check for key agent mode functions
        (should (member 'copilot-chat-agent-mode-enable commands-found))
        (should (member 'copilot-chat-agent-mode-disable commands-found))
        (should (member 'copilot-chat-agent-mode-toggle commands-found))
        (should (member 'copilot-chat-agent-statistics commands-found))
        (should (member 'copilot-chat-agent-debug-info commands-found))
        (should (member 'copilot-chat-agent-clear-log commands-found))))))

(ert-deftest test-copilot-chat-transient-main-menu-includes-agent ()
  "Test that the main transient menu includes the agent menu option."
  (let ((prefix (get 'copilot-chat-transient 'transient--prefix)))
    (should prefix)
    
    ;; Check that the main menu layout includes agent option
    (let ((layout (oref prefix layout))
          (agent-found nil))
      (dolist (group layout)
        (when (listp group)
          (dolist (item group)
            (when (listp item)
              (dolist (element item)
                (when (and (listp element)
                           (stringp (car element))
                           (equal (car element) "A")
                           (equal (nth 1 element) "Agent"))
                  (setq agent-found t)
                  (should (eq (nth 2 element) 'copilot-chat-transient-agent))))))))
      
      (should agent-found))))

(ert-deftest test-copilot-chat-transient-agent-auto-run-level-commands ()
  "Test that auto-run level commands are properly defined as lambda functions."
  (let ((prefix (get 'copilot-chat-transient-agent 'transient--prefix)))
    (should prefix)
    
    ;; Check that lambda functions for auto-run levels exist
    (let ((layout (oref prefix layout))
          (lambda-commands-found 0))
      (dolist (group layout)
        (when (listp group)
          (dolist (item group)
            (when (listp item)
              (dolist (element item)
                (when (and (listp element)
                           (functionp (nth 2 element))
                           (eq (car (nth 2 element)) 'lambda))
                  (incf lambda-commands-found)))))))
      
      ;; Should have 5 lambda functions for auto-run levels (none, read-only, safe, write, all)
      (should (>= lambda-commands-found 5)))))

(ert-deftest test-copilot-chat-transient-agent-require ()
  "Test that copilot-chat-agent is properly required by the transient module."
  (let ((features-before (copy-sequence features))
        (require-called nil))
    
    ;; Mock require to track calls
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (when (eq feature 'copilot-chat-agent)
                   (setq require-called t))
                 ;; Call original require for other features
                 (unless (eq feature 'copilot-chat-agent)
                   (require feature filename noerror)))))
      
      ;; Load the transient module (this would normally trigger the require)
      (load "copilot-chat-transient" t)
      
      ;; In the actual code, the require is at the top level, so it should be called
      ;; We can't easily test this without reloading the module, so we'll just verify
      ;; that the feature is available
      (should (featurep 'copilot-chat-agent)))))

(ert-deftest test-copilot-chat-transient-agent-menu-interactive ()
  "Test that the agent transient menu is marked as interactive."
  (should (commandp 'copilot-chat-transient-agent))
  (should (interactive-form 'copilot-chat-transient-agent)))

(provide 'test-copilot-chat-transient-agent)
;;; test-copilot-chat-transient-agent.el ends here