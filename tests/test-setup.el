;;; tests/test-setup.el --- Minimal setup for running tests -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets up the minimal test environment for copilot-chat-agent tests.

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path ".")

;; Load cl-lib for cl-proclaim
(require 'cl-lib)

;; Mock polymode if not available to avoid dependency issues
(unless (featurep 'polymode)
  (provide 'polymode))

;; Load core modules
(require 'copilot-chat-instance)

(message "Test setup complete. Core modules loaded.")

;;; test-setup.el ends here