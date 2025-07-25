;;; copilot-chat --- copilot-chat-transient.el  --- copilot chat transient functions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  copilot-chat maintainers

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


;;; Code:

(require 'transient)

(require 'copilot-chat-command)
(require 'copilot-chat-agent)

;;;###autoload (autoload 'copilot-chat-transient "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient () "Copilot chat command menu."
 [["Commands"
   ("d" "Display chat" copilot-chat-display)
   ("h" "Hide chat" copilot-chat-hide)
   ("x" "Reset" copilot-chat-reset)
   ("g" "Go to buffer" copilot-chat-switch-to-buffer)
   ("q" "Quit" transient-quit-one)]
  ["Instance"
   ("M" "Set model" copilot-chat-set-model)
   ("C" "Set commit model" copilot-chat-set-commit-model)
   ("S" "Save chat" copilot-chat-save)
   ("L" "Load chat" copilot-chat-load)
   ("k" "Kill instance" copilot-chat-kill-instance)]
  ["Actions"
   ("p" "Custom prompt" copilot-chat-custom-prompt-selection)
   ("i" "Ask and insert" copilot-chat-ask-and-insert)
   ("m" "Insert commit message" copilot-chat-insert-commit-message)]
  ["Data"
   ("y" "Yank last code block" copilot-chat-yank)
   ("s" "Send code to buffer" copilot-chat-send-to-buffer)]
  ["Tools"
   ("b" "Buffers" copilot-chat-transient-buffers)
   ("c" "Code helpers" copilot-chat-transient-code)
   ("A" "Agent" copilot-chat-transient-agent)]])

;;;###autoload (autoload 'copilot-chat-transient-buffers "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient-buffers () "Copilot chat buffers menu."
 [["Buffers"
   ("a" "Add buffers" copilot-chat-add-buffers)
   ("A"
    "Add all buffers in current frame"
    copilot-chat-add-buffers-in-current-window)
   ("d" "Delete buffers" copilot-chat-del-buffers)
   ("D" "Delete all buffers" copilot-chat-list-clear-buffers)
   ("f" "Add files under current directory" copilot-chat-add-files-under-dir)
   ("l" "Display buffer list" copilot-chat-list)
   ("c" "Clear buffers" copilot-chat-list-clear-buffers)
   ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'copilot-chat-transient-code "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient-code () "Copilot chat code helpers menu."
 [["Code helpers"
   ("e" "Explain" copilot-chat-explain)
   ("E" "Explain symbol" copilot-chat-explain-symbol-at-line)
   ("r" "Review" copilot-chat-review)
   ("d" "Doc" copilot-chat-doc)
   ("f" "Fix" copilot-chat-fix)
   ("o" "Optimize" copilot-chat-optimize)
   ("t" "Test" copilot-chat-test)
   ("F" "Explain function" copilot-chat-explain-defun)
   ("c" "Custom prompt function" copilot-chat-custom-prompt-function)
   ("R" "Review whole buffer" copilot-chat-review-whole-buffer)
   ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'copilot-chat-transient-agent "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient-agent () "Copilot chat agent menu."
 [["Agent Mode"
   ("e" "Enable agent mode" copilot-chat-agent-mode-enable)
   ("d" "Disable agent mode" copilot-chat-agent-mode-disable)
   ("t" "Toggle agent mode" copilot-chat-agent-mode-toggle)]
  ["Auto-run Level"
   ("n" "None" (lambda () (interactive) (copilot-chat-agent-set-auto-run-level 'none)))
   ("r" "Read-only" (lambda () (interactive) (copilot-chat-agent-set-auto-run-level 'read-only)))
   ("s" "Safe" (lambda () (interactive) (copilot-chat-agent-set-auto-run-level 'safe)))
   ("w" "Write" (lambda () (interactive) (copilot-chat-agent-set-auto-run-level 'write)))
   ("a" "All (dangerous)" (lambda () (interactive) (copilot-chat-agent-set-auto-run-level 'all)))]
  ["Information"
   ("S" "Statistics" copilot-chat-agent-statistics)
   ("D" "Debug info" copilot-chat-agent-debug-info)
   ("C" "Clear log" copilot-chat-agent-clear-log)
   ("q" "Quit" transient-quit-one)]])

(provide 'copilot-chat-transient)
;;; copilot-chat-transient.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
