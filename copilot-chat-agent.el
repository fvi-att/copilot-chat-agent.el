;;; copilot-chat-agent.el --- Copilot chat agent mode -*- lexical-binding: t -*-

;; Copyright (C) 2024  copilot-chat maintainers

;; Author: copilot-chat maintainers
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Copilot Chat Agent Mode - Automated command execution for Copilot Chat
;;
;; This module provides agent functionality that can automatically execute
;; commands suggested by GitHub Copilot in chat responses.  It includes
;; safety features, command classification, and user confirmation systems.

;;; Code:

(require 'cl-lib)
(require 'copilot-chat-common)
(require 'copilot-chat-instance)

;;; Customization

(defgroup copilot-chat-agent nil
  "Agent mode for Copilot Chat."
  :group 'copilot-chat)

(defcustom copilot-chat-agent-mode nil
  "Enable agent mode for automatic command execution."
  :type 'boolean
  :group 'copilot-chat-agent)

(defcustom copilot-chat-agent-auto-run-level 'read-only
  "Auto-run level for command execution.
- none: No automatic execution
- read-only: Only read-only commands (ls, cat, git status, etc.)
- safe: Safe commands that don't destroy existing files
- write: Commands that can modify files (with restrictions)
- all: All commands (dangerous, not recommended)"
  :type '(choice (const :tag "None" none)
                 (const :tag "Read-only" read-only)
                 (const :tag "Safe" safe)
                 (const :tag "Write" write)
                 (const :tag "All (dangerous)" all))
  :group 'copilot-chat-agent)

(defcustom copilot-chat-agent-timeout 30
  "Timeout for command execution in seconds."
  :type 'integer
  :group 'copilot-chat-agent)

(defcustom copilot-chat-agent-working-directory nil
  "Working directory for command execution.
If nil, use the instance directory."
  :type '(choice (const :tag "Auto (instance directory)" nil)
                 (directory :tag "Directory"))
  :group 'copilot-chat-agent)

;;; Command Classification

(defvar copilot-chat-agent-read-only-commands
  '("ls" "cat" "head" "tail" "grep" "find" "locate"
    "git status" "git log" "git diff" "git show"
    "ps" "top" "df" "du" "free" "uptime"
    "which" "whereis" "man" "help"
    "echo" "printf" "date" "pwd" "whoami"
    "wc" "sort" "uniq" "awk" "cut"
    "file" "stat" "lsof")
  "List of read-only commands that are safe to execute automatically.")

(defvar copilot-chat-agent-safe-commands
  '("mkdir" "touch" "cp" "ln"
    "git add" "git commit" "git push" "git pull"
    "npm install" "npm run build" "npm test"
    "pip install" "make" "make build" "make test"
    "emacs" "vim" "nano")
  "List of safe commands that create or build but don't destroy.")

(defvar copilot-chat-agent-write-commands
  '("rename"
    "git rm" "git mv" "git reset"
    "npm uninstall" "pip uninstall"
    "make clean" "make install")
  "List of write commands that can modify or remove files.")

(defvar copilot-chat-agent-dangerous-commands
  '("chmod" "chown" "mv" "rm" "rmdir" "dd" "mkfs" "fdisk"
    "sudo" "su" "passwd" "useradd" "userdel"
    "wget" "curl" "ssh" "scp" "rsync"
    "kill" "killall" "pkill"
    "iptables" "systemctl" "service")
  "List of dangerous commands that require careful consideration.")

(defvar copilot-chat-agent-forbidden-commands
  '("rm -rf /" "mkfs" "dd of=/dev/" ":(){ :|:& };:"
    "sudo rm -rf" "chmod 777 /" "chown -R"
    "wget | sh" "curl | sh" "eval"
    "format" "deltree")
  "List of forbidden commands that should never be executed automatically.")

;;; Session State

(defvar copilot-chat-agent-session-permissions nil
  "Temporary permissions granted during current session.")

(defvar copilot-chat-agent-execution-log nil
  "Log of executed commands.")

(defvar copilot-chat-agent-error-count 0
  "Number of execution errors in current session.")

;;; Core Functions

(defun copilot-chat-agent--classify-command (command)
  "Classify COMMAND into safety categories.
Returns one of: read-only, safe, write, dangerous, forbidden."
  (let ((trimmed-command (string-trim command)))
    (cond
     ;; 1. Check for explicitly forbidden commands (exact match first)
     ((member trimmed-command copilot-chat-agent-forbidden-commands)
      'forbidden)
     ((cl-some (lambda (cmd) (string-prefix-p cmd trimmed-command))
               copilot-chat-agent-forbidden-commands) 'forbidden)

     ;; 2. Check for dangerous patterns in pipes and exec
     ((string-match-p "xargs +\\(rm\\|kill\\|mv\\)" trimmed-command)
      'dangerous)
     ((string-match-p "xargs +kill +-[0-9]+" trimmed-command)
      'dangerous)
     ((string-match-p "-exec +\\(rm\\|kill\\|mv\\)" trimmed-command)
      'dangerous)
     ((string-match-p "|\\s*\\(sh\\|bash\\|zsh\\|csh\\)\\b" trimmed-command)
      'dangerous)

     ;; 3. Check for dangerous redirection patterns (writing to system files)
     ((string-match-p "> */etc/" trimmed-command)
      'dangerous)
     ((string-match-p "> */dev/" trimmed-command)
      'dangerous)

     ;; 4. Check for safe redirection (output redirection implies safe file creation)
     ((string-match-p ">" trimmed-command)
      'safe)

     ;; 4. Classify based on command matching (including compound commands)
     (t
      (let ((best-match nil)
            (best-category nil))
        ;; Check all command lists and find the longest matching prefix
        (dolist (cmd-list (list 
                           (cons copilot-chat-agent-dangerous-commands 'dangerous)
                           (cons copilot-chat-agent-write-commands 'write)
                           (cons copilot-chat-agent-safe-commands 'safe)
                           (cons copilot-chat-agent-read-only-commands 'read-only)))
          (dolist (cmd (car cmd-list))
            (when (string-prefix-p cmd trimmed-command)
              (when (or (null best-match) (> (length cmd) (length best-match)))
                (setq best-match cmd
                      best-category (cdr cmd-list))))))
        
        ;; Return the best match, or dangerous if no match found
        (or best-category 'dangerous))))))

(defun copilot-chat-agent--should-auto-execute-p (command)
  "Return non-nil if COMMAND should be executed automatically."
  (let ((category (copilot-chat-agent--classify-command command))
        (level copilot-chat-agent-auto-run-level))
    (pcase level
      ('none nil)
      ('read-only (eq category 'read-only))
      ('safe (memq category '(read-only safe)))
      ('write (memq category '(read-only safe write)))
      ('all (not (eq category 'forbidden)))
      (_ nil))))

(defun copilot-chat-agent--validate-command (command)
  "Validate COMMAND for safety.
Returns nil if command is safe, error message if not."
  (cond
   ((eq (copilot-chat-agent--classify-command command) 'forbidden)
    "Command is forbidden for security reasons")
   ((string-match-p "\\.\\./" command)
    "Path traversal detected")
   ((string-match-p "^\\s*$" command)
    "Empty command")
   (t nil)))

(defun copilot-chat-agent--extract-commands (text)
  "Extract shell commands from TEXT.
Looks for code blocks marked as shell, bash, or sh."
  (let ((commands '())
        (start 0))
    ;; Extract from markdown code blocks (```shell, ```bash, ```sh)
    (while (string-match "```\\(?:shell\\|bash\\|sh\\)?\n\\([^`]+\\)\n```" text start)
      (let ((code-block (match-string 1 text)))
        (dolist (line (split-string code-block "\n"))
          (let ((trimmed (string-trim line)))
            (when (and (not (string-empty-p trimmed))
                       (not (string-prefix-p "#" trimmed)))
              (push trimmed commands))))
        (setq start (match-end 0))))
    
    ;; Extract from org-mode code blocks (#+BEGIN_SRC shell) - simple approach
    (let ((lines (split-string text "\n"))
          (in-shell-block nil))
      (dolist (line lines)
        (cond
         ;; Start of shell block
         ((string-match "^[ \t]*#\\+BEGIN_SRC[ \t]+\\(?:shell\\|bash\\|sh\\)" line)
          (setq in-shell-block t))
         ;; End of block
         ((string-match "^[ \t]*#\\+END_SRC" line)
          (setq in-shell-block nil))
         ;; Command line inside shell block
         ((and in-shell-block
               (not (string-match "^[ \t]*$" line))
               (not (string-prefix-p "#" (string-trim line))))
          (let ((trimmed (string-trim line)))
            (when (not (string-empty-p trimmed))
              (push trimmed commands)))))))
    
    (reverse commands)))

(defun copilot-chat-agent--get-working-directory (instance)
  "Get working directory for command execution in INSTANCE."
  (or copilot-chat-agent-working-directory
      (copilot-chat-directory instance)
      default-directory))

(defun copilot-chat-agent--get-full-response (instance)
  "Get the full response text from INSTANCE.
Works with different backends (curl, request)."
  (let ((backend (copilot-chat--backend instance)))
    (cond
     ;; For curl backend
     ((and (fboundp 'copilot-chat-curl-answer)
           (copilot-chat-curl-answer backend))
      (copilot-chat-curl-answer backend))
     ;; For request backend - try to get from buffer
     ((copilot-chat-chat-buffer instance)
      (with-current-buffer (copilot-chat-chat-buffer instance)
        (let ((content (buffer-string)))
          ;; Extract only the last response (after last prompt)
          (if (string-match "\\*\\* \\*\[.*\\].* Copilot.*\\n\\([\\s\\S]*\\)\\'" content)
              (match-string 1 content)
            content))))
     ;; Fallback - return empty string
     (t ""))))

;;; Command Execution

(defun copilot-chat-agent--execute-command (command instance)
  "Execute COMMAND in INSTANCE context.
Returns a list (exit-code output error-output)."
  (let* ((default-directory (copilot-chat-agent--get-working-directory instance))
         (start-time (current-time))
         (process-environment process-environment)
         result)
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command command nil t)))
        (setq result (list exit-code 
                          (buffer-string)
                          (current-time-string)))
        (copilot-chat-agent--log-execution command result start-time)
        result))))

(defun copilot-chat-agent--log-execution (command result start-time)
  "Log execution of COMMAND with RESULT and START-TIME."
  (push (list :timestamp start-time
              :command command
              :exit-code (car result)
              :output (cadr result)
              :category (copilot-chat-agent--classify-command command)
              :auto-executed t)
        copilot-chat-agent-execution-log))

;;; User Interaction

(defun copilot-chat-agent--confirm-command (command)
  "Ask user for confirmation to execute COMMAND.
Returns one of: execute, skip, always-yes, quit."
  (let ((category (copilot-chat-agent--classify-command command)))
    (message "Execute command [%s]: %s" category command)
    (let ((choice (read-char-choice 
                   "Execute? (y)es, (n)o, (a)lways yes, (q)uit: "
                   '(?y ?n ?a ?q))))
      (pcase choice
        (?y 'execute)
        (?n 'skip) 
        (?a 'always-yes)
        (?q 'quit)
        (_ 'skip)))))

;;; Main Agent Functions

(defun copilot-chat-agent-mode-enable ()
  "Enable Copilot Chat agent mode."
  (interactive)
  (setq copilot-chat-agent-mode t)
  (message "Copilot Chat agent mode enabled"))

(defun copilot-chat-agent-mode-disable ()
  "Disable Copilot Chat agent mode."
  (interactive)
  (setq copilot-chat-agent-mode nil)
  (setq copilot-chat-agent-session-permissions nil)
  (message "Copilot Chat agent mode disabled"))

(defun copilot-chat-agent-mode-toggle ()
  "Toggle Copilot Chat agent mode."
  (interactive)
  (if copilot-chat-agent-mode
      (copilot-chat-agent-mode-disable)
    (copilot-chat-agent-mode-enable)))

(defun copilot-chat-agent-mode-p ()
  "Return non-nil if agent mode is enabled."
  copilot-chat-agent-mode)

(defun copilot-chat-agent-set-auto-run-level (level)
  "Set auto-run LEVEL for command execution."
  (interactive (list (intern (completing-read 
                              "Auto-run level: "
                              '("none" "read-only" "safe" "write" "all")
                              nil t))))
  (setq copilot-chat-agent-auto-run-level level)
  (message "Auto-run level set to: %s" level))

(defun copilot-chat-agent-process-response (response instance)
  "Process RESPONSE from Copilot in INSTANCE context.
Extract and potentially execute commands from the response."
  (when (and copilot-chat-agent-mode response instance)
    (let ((commands (copilot-chat-agent--extract-commands response)))
      (when commands
        (copilot-chat-agent--handle-commands commands instance)))))

(defun copilot-chat-agent--handle-commands (commands instance)
  "Handle list of COMMANDS in INSTANCE context."
  (let ((continue t))
    (dolist (command commands)
      (when continue
        (let ((validation-error (copilot-chat-agent--validate-command command)))
          (if validation-error
              (message "Command validation failed: %s - %s" command validation-error)
            (setq continue (copilot-chat-agent--handle-single-command command instance))))))))

(defun copilot-chat-agent--handle-single-command (command instance)
  "Handle single COMMAND in INSTANCE context.
Returns non-nil to continue processing, nil to stop."
  (cond
   ((copilot-chat-agent--should-auto-execute-p command)
    (copilot-chat-agent--execute-and-display command instance)
    t)
   (t
    (when (and instance (copilot-chat-chat-buffer instance))
      (copilot-chat--write-buffer instance
                                (format "\n*** Confirm to execute command:\n#+BEGIN_SRC shell\n%s\n#+END_SRC\n" command)
                                nil))
    (let ((choice (copilot-chat-agent--confirm-command command)))
      (pcase choice
        ('execute
         (copilot-chat-agent--execute-and-display command instance)
         t)
        ('skip
         (when (and instance (copilot-chat-chat-buffer instance))
           (copilot-chat--write-buffer instance "\n*** Skipped command execution.\n" nil))
         t)
        ('always-yes
         (push command copilot-chat-agent-session-permissions)
         (copilot-chat-agent--execute-and-display command instance)
         t)
        ('quit
         (when (and instance (copilot-chat-chat-buffer instance))
           (copilot-chat--write-buffer instance "\n*** Aborted command execution.\n" nil))
         nil)
        (_ t))))))

(defun copilot-chat-agent--execute-and-display (command instance)
  "Execute COMMAND and display results in INSTANCE context."
  (message "Executing: %s" command)
  (let ((result (copilot-chat-agent--execute-command command instance)))
    (let ((exit-code (nth 0 result))
          (output (nth 1 result)))
      ;; Display execution info in chat buffer  
      (when (and instance (copilot-chat-chat-buffer instance))
        (let* ((trimmed-output (if (string-empty-p output) "(no output)" (string-trim output)))
               (status-icon (if (zerop exit-code) "✅" "❌"))
               (result-text (if (zerop exit-code)
                               (format "\n*** %s Agent executed: ~%s~\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                                      status-icon command trimmed-output)
                             (format "\n*** %s Agent failed: ~%s~ (exit code: %d)\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                                    status-icon command exit-code trimmed-output))))
          (copilot-chat--write-buffer instance result-text nil)))
      
      ;; Also show brief message
      (if (zerop exit-code)
          (message "✓ Executed: %s" command)
        (message "✗ Failed: %s (exit code %d)" command exit-code)
        (cl-incf copilot-chat-agent-error-count)))))

;;; Statistics and Debugging

(defun copilot-chat-agent-statistics ()
  "Display execution statistics."
  (interactive)
  (let ((total (length copilot-chat-agent-execution-log))
        (auto-count (cl-count-if 
                     (lambda (entry) (plist-get entry :auto-executed))
                     copilot-chat-agent-execution-log)))
    (if (zerop total)
        (message "No commands executed yet")
      (message "Total commands: %d, Auto-executed: %d (%.1f%%)"
               total auto-count (* 100.0 (/ (float auto-count) total))))))

(defun copilot-chat-agent-clear-log ()
  "Clear execution log."
  (interactive)
  (setq copilot-chat-agent-execution-log nil)
  (setq copilot-chat-agent-error-count 0)
  (message "Execution log cleared"))

(defun copilot-chat-agent-debug-info ()
  "Display debug information."
  (interactive)
  (with-current-buffer (get-buffer-create "*copilot-agent-debug*")
    (erase-buffer)
    (insert (format "Agent mode: %s\n" 
                    (if copilot-chat-agent-mode "enabled" "disabled")))
    (insert (format "Auto-run level: %s\n" copilot-chat-agent-auto-run-level))
    (insert (format "Session permissions: %s\n" copilot-chat-agent-session-permissions))
    (insert (format "Recent executions: %d\n" (length copilot-chat-agent-execution-log)))
    (insert (format "Error count: %d\n" copilot-chat-agent-error-count))
    (insert (format "Working directory: %s\n" 
                    (or copilot-chat-agent-working-directory "auto")))
    (pop-to-buffer (current-buffer))))

(provide 'copilot-chat-agent)
;;; copilot-chat-agent.el ends here
