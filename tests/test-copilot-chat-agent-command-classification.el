;;; test-copilot-chat-agent-command-classification.el --- Tests for command classification -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains tests for the command classification logic in the
;; copilot-chat-agent module, based on the requirements specified in
;; docs/copilot-chat-agent要件仕様書.md.

;;; Code:

(require 'ert)
(require 'copilot-chat-agent)

;; Helper function to access the internal classification function.
;; We assume the function is named `copilot-chat-agent--classify-command`.
;; If the function name is different, this helper should be updated.
(defun test-helper-classify-command (command)
  "A test wrapper for the internal `copilot-chat-agent--classify-command`."
  (copilot-chat-agent--classify-command command))

(ert-deftest test-command-classification-read-only ()
  "Test classification of read-only commands."
  (should (eq (test-helper-classify-command "ls -la") 'read-only))
  (should (eq (test-helper-classify-command "cat file.txt") 'read-only))
  (should (eq (test-helper-classify-command "head -n 10 file") 'read-only))
  (should (eq (test-helper-classify-command "tail -f /var/log/syslog") 'read-only))
  (should (eq (test-helper-classify-command "grep 'pattern' file") 'read-only))
  (should (eq (test-helper-classify-command "find . -name '*.el'") 'read-only))
  (should (eq (test-helper-classify-command "git status") 'read-only))
  (should (eq (test-helper-classify-command "git log --oneline") 'read-only))
  (should (eq (test-helper-classify-command "git diff --staged") 'read-only))
  (should (eq (test-helper-classify-command "ps aux") 'read-only))
  (should (eq (test-helper-classify-command "df -h") 'read-only))
  (should (eq (test-helper-classify-command "du -sh /path") 'read-only))
  (should (eq (test-helper-classify-command "which emacs") 'read-only))
  (should (eq (test-helper-classify-command "whereis ls") 'read-only)))

(ert-deftest test-command-classification-safe ()
  "Test classification of safe commands."
  (should (eq (test-helper-classify-command "mkdir new_directory") 'safe))
  (should (eq (test-helper-classify-command "touch new_file.txt") 'safe))
  (should (eq (test-helper-classify-command "cp source.txt destination.txt") 'safe))
  (should (eq (test-helper-classify-command "git add .") 'safe))
  (should (eq (test-helper-classify-command "git commit -m 'Initial commit'") 'safe))
  ;; Note: These could be considered 'dangerous' in some contexts,
  ;; but based on the spec, they are 'safe'.
  (should (eq (test-helper-classify-command "npm install lodash") 'safe))
  (should (eq (test-helper-classify-command "pip install -r requirements.txt") 'safe)))

(ert-deftest test-command-classification-dangerous ()
  "Test classification of dangerous commands."
  (should (eq (test-helper-classify-command "rm file.txt") 'dangerous))
  (should (eq (test-helper-classify-command "rm -rf /") 'forbidden)) ; This is forbidden, not just dangerous
  (should (eq (test-helper-classify-command "mv old new") 'dangerous))
  (should (eq (test-helper-classify-command "chmod 777 file") 'dangerous))
  (should (eq (test-helper-classify-command "chown user:group file") 'dangerous))
  (should (eq (test-helper-classify-command "sudo apt-get update") 'dangerous))
  (should (eq (test-helper-classify-command "curl -X POST http://example.com") 'dangerous))
  (should (eq (test-helper-classify-command "wget https://example.com/file.zip") 'dangerous))
  (should (eq (test-helper-classify-command "echo 'dangerous' > /etc/hosts") 'dangerous)))

(ert-deftest test-command-classification-unknown ()
  "Test classification of unknown commands."
  ;; Unknown commands should probably default to 'dangerous' for safety.
  (should (eq (test-helper-classify-command "my_custom_script.sh") 'dangerous))
  (should (eq (test-helper-classify-command "unknown-command") 'dangerous)))

(ert-deftest test-command-classification-with-complex-commands ()
  "Test classification of complex commands with pipes and redirects."
  (should (eq (test-helper-classify-command "ls -l | grep .el | wc -l") 'read-only))
  (should (eq (test-helper-classify-command "cat file | sort > sorted_file") 'safe))
  (should (eq (test-helper-classify-command "ps aux | grep 'emacs' | awk '{print $2}' | xargs kill -9") 'dangerous))
  (should (eq (test-helper-classify-command "find . -type f -exec rm {} \;") 'dangerous)))

(provide 'test-copilot-chat-agent-command-classification)
;;; test-copilot-chat-agent-command-classification.el ends here
