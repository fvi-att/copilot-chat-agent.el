;;; test-copilot-chat-prompt-mode-agent.el --- Tests for copilot-chat-prompt-mode agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agent mode integration in copilot-chat-prompt-mode.el

;;; Code:

(require 'ert)
(require 'copilot-chat-prompt-mode)
(require 'copilot-chat-agent)

(ert-deftest test-copilot-chat-prompt-mode-agent-processing ()
  "Test that agent response processing is called when agent mode is enabled."
  (let ((copilot-chat-agent-mode t)
        (instance (make-copilot-chat-instance))
        (agent-process-called nil)
        (mock-response "Test response"))
    
    ;; Mock the agent functions
    (cl-letf (((symbol-function 'copilot-chat-agent--get-full-response)
               (lambda (_instance) mock-response))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (response inst)
                 (setq agent-process-called t)
                 (should (equal response mock-response))
                 (should (equal inst instance)))))
      
      ;; Set up spinner to be stopped
      (setf (copilot-chat--spinner-timer instance) t)
      
      ;; Mock spinner stop function
      (cl-letf (((symbol-function 'copilot-chat--spinner-stop)
                 (lambda (_) nil)))
        
        ;; Call the function that should trigger agent processing
        (copilot-chat--write-buffer-end-of-stream instance "test content")
        
        ;; Verify agent processing was called
        (should agent-process-called)))))

(ert-deftest test-copilot-chat-prompt-mode-no-agent-processing-when-disabled ()
  "Test that agent response processing is not called when agent mode is disabled."
  (let ((copilot-chat-agent-mode nil)
        (instance (make-copilot-chat-instance))
        (agent-process-called nil))
    
    ;; Mock the agent functions
    (cl-letf (((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _inst)
                 (setq agent-process-called t))))
      
      ;; Set up spinner to be stopped
      (setf (copilot-chat--spinner-timer instance) t)
      
      ;; Mock spinner stop function
      (cl-letf (((symbol-function 'copilot-chat--spinner-stop)
                 (lambda (_) nil)))
        
        ;; Call the function
        (copilot-chat--write-buffer-end-of-stream instance "test content")
        
        ;; Verify agent processing was NOT called
        (should-not agent-process-called)))))

(ert-deftest test-copilot-chat-prompt-mode-agent-require ()
  "Test that copilot-chat-agent is properly required when needed."
  (let ((copilot-chat-agent-mode t)
        (instance (make-copilot-chat-instance))
        (require-called nil))
    
    ;; Mock require function
    (cl-letf (((symbol-function 'require)
               (lambda (feature)
                 (when (eq feature 'copilot-chat-agent)
                   (setq require-called t))
                 t))
              ((symbol-function 'copilot-chat-agent--get-full-response)
               (lambda (_) "test"))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _inst) nil)))
      
      ;; Set up spinner
      (setf (copilot-chat--spinner-timer instance) t)
      
      ;; Mock spinner stop function
      (cl-letf (((symbol-function 'copilot-chat--spinner-stop)
                 (lambda (_) nil)))
        
        ;; Call the function
        (copilot-chat--write-buffer-end-of-stream instance "test content")
        
        ;; Verify require was called
        (should require-called)))))

(provide 'test-copilot-chat-prompt-mode-agent)
;;; test-copilot-chat-prompt-mode-agent.el ends here