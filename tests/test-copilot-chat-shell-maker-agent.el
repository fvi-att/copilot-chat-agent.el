;;; test-copilot-chat-shell-maker-agent.el --- Tests for copilot-chat-shell-maker agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the agent mode integration in copilot-chat-shell-maker.el

;;; Code:

(require 'ert)
(require 'copilot-chat-shell-maker)
(require 'copilot-chat-agent)

(ert-deftest test-copilot-chat-shell-maker-agent-processing ()
  "Test that agent response processing is called when agent mode is enabled in shell-maker."
  (let ((copilot-chat-agent-mode t)
        (instance (make-copilot-chat-instance))
        (agent-process-called nil)
        (mock-response "Test shell response"))
    
    ;; Mock the backend and curl answer
    (setf (copilot-chat--backend instance) 'mock-backend)
    
    ;; Mock the agent functions
    (cl-letf (((symbol-function 'copilot-chat-curl-answer)
               (lambda (_backend) mock-response))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (response inst)
                 (setq agent-process-called t)
                 (should (equal response mock-response))
                 (should (equal inst instance)))))
      
      ;; Mock shell-maker functions
      (let ((shell (make-hash-table)))
        (map-put shell :finish-output (lambda (_) t))
        
        ;; Mock shell-maker functions
        (cl-letf (((symbol-function 'copilot-chat--shell-maker-copy-faces)
                   (lambda (_) nil))
                  ((symbol-function 'copilot-chat--shell-maker-finalize)
                   (lambda (_) nil)))
          
          ;; Call the function that should trigger agent processing
          (copilot-chat--shell-maker-output-end instance shell)
          
          ;; Verify agent processing was called
          (should agent-process-called))))))

(ert-deftest test-copilot-chat-shell-maker-no-agent-processing-when-disabled ()
  "Test that agent response processing is not called when agent mode is disabled in shell-maker."
  (let ((copilot-chat-agent-mode nil)
        (instance (make-copilot-chat-instance))
        (agent-process-called nil))
    
    ;; Mock the agent functions
    (cl-letf (((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _inst)
                 (setq agent-process-called t))))
      
      ;; Mock shell-maker functions
      (let ((shell (make-hash-table)))
        (map-put shell :finish-output (lambda (_) t))
        
        ;; Mock shell-maker functions
        (cl-letf (((symbol-function 'copilot-chat--shell-maker-copy-faces)
                   (lambda (_) nil))
                  ((symbol-function 'copilot-chat--shell-maker-finalize)
                   (lambda (_) nil)))
          
          ;; Call the function
          (copilot-chat--shell-maker-output-end instance shell)
          
          ;; Verify agent processing was NOT called
          (should-not agent-process-called))))))

(ert-deftest test-copilot-chat-shell-maker-agent-require ()
  "Test that copilot-chat-agent is properly required when needed in shell-maker."
  (let ((copilot-chat-agent-mode t)
        (instance (make-copilot-chat-instance))
        (require-called nil))
    
    ;; Mock backend
    (setf (copilot-chat--backend instance) 'mock-backend)
    
    ;; Mock require function
    (cl-letf (((symbol-function 'require)
               (lambda (feature)
                 (when (eq feature 'copilot-chat-agent)
                   (setq require-called t))
                 t))
              ((symbol-function 'copilot-chat-curl-answer)
               (lambda (_) "test"))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _inst) nil)))
      
      ;; Mock shell-maker functions
      (let ((shell (make-hash-table)))
        (map-put shell :finish-output (lambda (_) t))
        
        ;; Mock shell-maker functions
        (cl-letf (((symbol-function 'copilot-chat--shell-maker-copy-faces)
                   (lambda (_) nil))
                  ((symbol-function 'copilot-chat--shell-maker-finalize)
                   (lambda (_) nil)))
          
          ;; Call the function
          (copilot-chat--shell-maker-output-end instance shell)
          
          ;; Verify require was called
          (should require-called))))))

(ert-deftest test-copilot-chat-shell-maker-first-word-answer-flag ()
  "Test that first-word-answer flag is properly set in shell-maker with agent mode."
  (let ((copilot-chat-agent-mode t)
        (instance (make-copilot-chat-instance)))
    
    ;; Mock backend
    (setf (copilot-chat--backend instance) 'mock-backend)
    (setf (copilot-chat-first-word-answer instance) nil)
    
    ;; Mock the agent functions
    (cl-letf (((symbol-function 'copilot-chat-curl-answer)
               (lambda (_) "test"))
              ((symbol-function 'copilot-chat-agent-process-response)
               (lambda (_response _inst) nil)))
      
      ;; Mock shell-maker functions
      (let ((shell (make-hash-table)))
        (map-put shell :finish-output (lambda (_) t))
        
        ;; Mock shell-maker functions
        (cl-letf (((symbol-function 'copilot-chat--shell-maker-copy-faces)
                   (lambda (_) nil))
                  ((symbol-function 'copilot-chat--shell-maker-finalize)
                   (lambda (_) nil)))
          
          ;; Call the function
          (copilot-chat--shell-maker-output-end instance shell)
          
          ;; Verify first-word-answer flag is set
          (should (copilot-chat-first-word-answer instance)))))))

(provide 'test-copilot-chat-shell-maker-agent)
;;; test-copilot-chat-shell-maker-agent.el ends here