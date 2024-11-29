;;; lsp.el --- lsp for go/python...:
;;; Commentary:
;;  we used lsp booster here.
;;; Code:
;; comments for command
;; ;; build gopls on mac 13.5, fetch the source code and :
;; CGO_ENABLED=1 go build -ldflags '-w -s "-extldflags=-lresolv -L/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/usr/lib -F/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/System/Library/Frameworks/" '

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)



;;; company
(yas-global-mode)
(global-company-mode t)
;; Navigate in completion minibuffer with `C-n` and `C-p`.
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
;; Provide instant autocompletion.
(setq company-idle-delay 0.0)

;; go-mode lsp
(add-hook 'go-mode-hook #'lsp-deferred)

;； go mode rename
;; go install golang.org/x/tools/cmd/gorename@latest
;; not working, don't know why.

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (setq gofmt-command "gofmt")
;; (add-hook 'before-save-hook #'gofmt-before-save)


;; lsp perf
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil) ; if set to true can cause a performance hit
(setq lsp-file-watch-threshold 2000)

;; lsp ui
(require 'lsp-ui)
(add-hook 'go-mode-hook #'lsp-ui-mode)


;; lsp-treemacs / treemacs
(lsp-treemacs-sync-mode 1)
(setq treemacs-project-follow-mode t)
(setq treemacs-display-current-project-exclusively t)
(global-set-key (kbd "C-'") 'lsp-treemacs-symbols)
(global-set-key (kbd "<f5>") 'treemacs)
(setq treemacs-filewatch-mode t)
(setq treemacs-git-mode 'simple)
(setq treemacs-git-commit-diff-mode t)

;; lsp booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)



;; python
(add-hook 'python-mode-hook #'lsp-deferred)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'lsp)
;;; lsp.el ends here
