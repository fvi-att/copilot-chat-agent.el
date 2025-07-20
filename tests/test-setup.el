;;; tests/test-setup.el --- Setup for running tests -*- lexical-binding: t; -*-

(require 'package)

;; Initialize the package system
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; List of dependencies to install
;; This list should be kept in sync with the Package-Requires header in copilot-chat.el
(defvar test-dependencies
  '(aio request transient polymode org markdown-mode shell-maker))

;; Install missing dependencies
(dolist (pkg test-dependencies)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(message "Test setup complete. All dependencies should be installed.")
