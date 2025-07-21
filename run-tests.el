;;; run-tests.el --- Run all tests for copilot-chat-agent -*- lexical-binding: t; -*-

;; Add project root to load-path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Setup packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; Install dependencies
(defvar test-dependencies
  '(aio request transient polymode org markdown-mode shell-maker))
(dolist (pkg test-dependencies)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Load required project files before compilation and tests
(require 'copilot-chat-common)
(require 'copilot-chat-instance)

;; Byte-compile the project
(byte-recompile-directory (file-name-directory (or load-file-name buffer-file-name)) 0 t)

;; Load all test files
(load-file "tests/test-copilot-chat-agent-command-classification.el")
(load-file "tests/test-copilot-chat-main-agent.el")
(load-file "tests/test-copilot-chat-prompt-mode-agent.el")
(load-file "tests/test-copilot-chat-shell-maker-agent.el")
;; (load-file "tests/test-copilot-chat-transient-agent.el") ;; Excluded for now

;; Run all tests
(ert-run-tests-batch-and-exit)
