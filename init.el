;; comments for comand
;; ;; build gopls on mac 13.5, fetch the source code and :
;; CGO_ENABLED=1 go build -ldflags '-w -s "-extldflags=-lresolv -L/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/usr/lib -F/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/System/Library/Frameworks/" '


(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))



;; line operation
(defun mark-from-point-to-end-of-line ()
  "Marks everything from point to end of line"
  (interactive)
  (set-mark (line-end-position))
  (activate-mark))

(global-set-key (kbd "C-x n f") 'mark-from-point-to-end-of-line)


(defun backward-copy-word ()
  (interactive)
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-word) (point)))))
(global-set-key (kbd "C-x n c") 'backward-copy-word)



;; company

(global-company-mode t)
;; Navigate in completion minibuffer with `C-n` and `C-p`.
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; Provide instant autocompletion.
(setq company-idle-delay 0.0)

;; full path
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq mac-command-modifier 'meta)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

; path settings
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                                    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


(cond ((eq system-type 'windows-nt)
        ;; Windows-specific code goes here.
       (add-to-list 'exec-path
		    "c:/Users/win/AppData/Roaming/Python/Python310/Scripts")
       (add-to-list 'exec-path "c:/Program Files/Git/bin")
       (add-to-list 'exec-path "c:/Python310/Scripts")
       (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin")
       (add-to-list 'exec-path "c:/Users/win/AppData/Local/Microsoft/WindowsApps")
       (setenv "GOROOT" "c:/Program Files/Go")
       (setenv "GOPATH" "d:/go")
       (add-to-list 'exec-path "c:/Program Files/go/bin")
       (add-to-list 'exec-path "d:/go/bin")
       (setenv "PATH" (mapconcat #'identity exec-path path-separator)))
      ((eq system-type 'darwin)
       ;; mac-specific code goes here.
       (setenv "GOROOT" "/usr/local/go")
       (setenv "GOPATH" "/Users/yayu/Golang")

       (add-to-list 'exec-path "~/Golang/bin")
       (if (file-exists-p "~/.go.env")
	   (load-env-vars "~/.go.env"))
       (setenv "PATH" (concat  "/usr/local/go/bin" ":" (getenv "PATH")))
       ))



;; go-mode lsp
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; flycheck
;; spell check
(add-hook 'after-init-hook #'global-flycheck-mode)


;; indent-guide
(indent-guide-global-mode)
(global-display-line-numbers-mode)
(global-hl-line-mode t)

;; helm
(require 'helm)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-full-path)
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")
;;(helm-recentf)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
(global-set-key (kbd "C-x C-b") 'recentf-open)



(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq make-backup-files nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages '(flycheck company go-mode exec-path-from-shell helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UbuntuMono Nerd Font Mono" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))




(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)
