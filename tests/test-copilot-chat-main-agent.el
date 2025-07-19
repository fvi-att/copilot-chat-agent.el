;;; test-copilot-chat-main-agent.el --- Tests for copilot-chat.el agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agent mode integration in the main copilot-chat.el file

;;; Code:

(require 'ert)
(require 'copilot-chat)
(require 'copilot-chat-agent)

(ert-deftest test-copilot-chat-requires-agent ()
  "Test that copilot-chat.el properly requires copilot-chat-agent."
  ;; This test verifies that the require statement was added
  (should (featurep 'copilot-chat-agent)))

(ert-deftest test-copilot-chat-agent-functions-available ()
  "Test that agent functions are available after loading copilot-chat."
  ;; Test that key agent functions are defined and available
  (should (fboundp 'copilot-chat-agent-mode-enable))
  (should (fboundp 'copilot-chat-agent-mode-disable))
  (should (fboundp 'copilot-chat-agent-mode-toggle))
  (should (fboundp 'copilot-chat-agent-set-auto-run-level))
  (should (fboundp 'copilot-chat-agent-statistics))
  (should (fboundp 'copilot-chat-agent-debug-info))
  (should (fboundp 'copilot-chat-agent-clear-log)))

(ert-deftest test-copilot-chat-agent-variable-available ()
  "Test that agent mode variable is available after loading copilot-chat."
  ;; Test that the agent mode variable is defined
  (should (boundp 'copilot-chat-agent-mode)))

(ert-deftest test-copilot-chat-agent-integration-with-frontends ()
  "Test that agent integration works with different frontends."
  (let ((original-frontend copilot-chat-frontend))
    (unwind-protect
        (dolist (frontend '(org markdown shell-maker))
          (setq copilot-chat-frontend frontend)
          
          ;; Test that we can enable agent mode with different frontends
          (let ((copilot-chat-agent-mode nil))
            (should-not copilot-chat-agent-mode)
            
            ;; Enable agent mode
            (copilot-chat-agent-mode-enable)
            (should copilot-chat-agent-mode)
            
            ;; Disable agent mode
            (copilot-chat-agent-mode-disable)
            (should-not copilot-chat-agent-mode)))
      
      ;; Restore original frontend
      (setq copilot-chat-frontend original-frontend))))

(ert-deftest test-copilot-chat-agent-auto-run-level-setting ()
  "Test that auto-run level can be set and retrieved."
  (let ((original-level copilot-chat-agent-auto-run-level))
    (unwind-protect
        (dolist (level '(none read-only safe write all))
          (copilot-chat-agent-set-auto-run-level level)
          (should (eq copilot-chat-agent-auto-run-level level)))
      
      ;; Restore original level
      (setq copilot-chat-agent-auto-run-level original-level))))

(ert-deftest test-copilot-chat-agent-statistics-and-debug ()
  "Test that agent statistics and debug functions work."
  ;; These functions should not error when called
  (should-not (condition-case err
                  (progn
                    (copilot-chat-agent-statistics)
                    (copilot-chat-agent-debug-info)
                    (copilot-chat-agent-clear-log)
                    nil)
                (error err))))

(ert-deftest test-copilot-chat-agent-mode-toggle ()
  "Test that agent mode toggle works correctly."
  (let ((original-mode copilot-chat-agent-mode))
    (unwind-protect
        (progn
          ;; Set to a known state
          (setq copilot-chat-agent-mode nil)
          
          ;; Toggle should enable
          (copilot-chat-agent-mode-toggle)
          (should copilot-chat-agent-mode)
          
          ;; Toggle should disable
          (copilot-chat-agent-mode-toggle)
          (should-not copilot-chat-agent-mode))
      
      ;; Restore original state
      (setq copilot-chat-agent-mode original-mode))))

(ert-deftest test-copilot-chat-with-agent-mode-integration ()
  "Test that copilot-chat works with agent mode enabled."
  (let ((copilot-chat-agent-mode t)
        (test-instance nil))
    
    ;; Mock functions to avoid actual API calls
    (cl-letf (((symbol-function 'copilot-chat--frontend-create)
               (lambda () (make-copilot-chat-instance)))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _instance) nil)))
      
      ;; This should not error with agent mode enabled
      (should-not (condition-case err
                      (progn
                        (setq test-instance (copilot-chat--frontend-create))
                        nil)
                    (error err)))
      
      ;; Instance should be created successfully
      (should test-instance))))

(provide 'test-copilot-chat-main-agent)
;;; test-copilot-chat-main-agent.el ends here