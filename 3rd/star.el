;;; ytt-starlark-support.el --- Support for Carvel YTT Starlark modules

;;; Commentary:
;; This file provides Emacs support for Carvel YTT Starlark modules (*.star).
;; It configures syntax highlighting for YAML and Starlark code, recognizing
;; Starlark comments (with # or #!) and treating everything else as Starlark code.

;;; Code:

;; Ensure that the necessary packages are installed
(require 'yaml-mode)

;; Define a custom function to enable starlark-mode for Starlark code blocks
(defun ytt-enable-starlark-mode ()
  "Enable starlark-mode inside YTT files where Starlark is used."
  (when (and (eq major-mode 'yaml-mode) (save-excursion
                                          (re-search-forward "\\`#\\|#!" nil t)))
    (starlark-mode)))

;; Add a hook to trigger starlark-mode inside .star files when appropriate
(add-hook 'yaml-mode-hook
          (lambda ()
            (when (and (buffer-file-name) 
                       (string-match-p "\\.star\\'" (buffer-file-name)))
              ;; Enable starlark-mode when we encounter a .star file
              (add-hook 'after-change-functions
                        (lambda (_beg _end _len)
                          (ytt-enable-starlark-mode)) nil t))))

;; Define a major mode for Starlark (simple Python-like syntax)
(define-derived-mode starlark-mode prog-mode "Starlark"
  "Major mode for editing Starlark code."
  ;; Syntax highlighting for Starlark
  (setq font-lock-defaults '(starlark-font-lock-keywords))
  (setq comment-start "# ")
  (setq comment-end ""))

;; Define simple syntax highlighting for Starlark (Python-like syntax)
(defvar starlark-font-lock-keywords
  '(;; Keywords (including end and return)
    ("\\<\\(def\\|if\\|else\\|for\\|while\\|break\\|continue\\|return\\|end\\)\\>" . font-lock-keyword-face)
    ;; Function names
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" . font-lock-function-name-face)
    ;; Strings
    ("\"[^\"]*\"" . font-lock-string-face)
    ;; Numbers
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    ;; Comments (handle both # and #! for comments)
    ("^#!?.*" . font-lock-comment-face) ;; Match # or #! at the start of the line
    ;; Built-in functions
    ("\\<\\(print\\|len\\|range\\|set\\|list\\|dict\\|type\\|bool\\|int\\|float\\|str\\)\\>" . font-lock-builtin-face)
    ))

;; Associate .star files with yaml-mode and ensure Starlark is highlighted inside
(add-to-list 'auto-mode-alist '("\\.star\\'" . yaml-mode))

;; Provide the feature for use in your Emacs configuration
(provide 'ytt-starlark-support)

;;; ytt-starlark-support.el ends here
