;;; debug-test.el --- Debug command extraction -*- lexical-binding: t -*-

(require 'package)
(package-initialize)
(add-to-list 'load-path ".")
(require 'copilot-chat-agent)

(defun debug-command-extraction ()
  "Debug command extraction step by step."
  (let ((simple-text "#+BEGIN_SRC shell\ncurl ifconfig.me\n#+END_SRC")
        (complex-text "*** Test

#+BEGIN_SRC shell
curl ifconfig.me
#+END_SRC

Text between

#+BEGIN_SRC shell
curl -s https://api.ipify.org
#+END_SRC"))
    
    (message "=== Debug Command Extraction ===")
    
    ;; Test simple case
    (message "\n--- Simple case ---")
    (message "Input: %s" simple-text)
    (let ((commands (copilot-chat-agent--extract-commands simple-text)))
      (message "Result: %s" commands))
    
    ;; Test complex case
    (message "\n--- Complex case ---")
    (message "Input: %s" complex-text)
    (let ((commands (copilot-chat-agent--extract-commands complex-text)))
      (message "Result: %s" commands))
    
    ;; Test regex step by step
    (message "\n--- Regex debugging ---")
    (let ((pattern "#\\+BEGIN_SRC \\(?:shell\\|bash\\|sh\\)\\s-*\n\\([\\s\\S]*?\\)#\\+END_SRC"))
      (message "Pattern: %s" pattern)
      (when (string-match pattern complex-text)
        (message "Match found at position: %d" (match-beginning 0))
        (message "Full match: %s" (match-string 0 complex-text))
        (message "Group 1: %s" (match-string 1 complex-text))))))

;; Run debug immediately
(debug-command-extraction)

(provide 'debug-test)
;;; debug-test.el ends here