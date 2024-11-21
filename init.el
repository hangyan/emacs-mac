;; comments for command
;; ;; build gopls on mac 13.5, fetch the source code and :
;; CGO_ENABLED=1 go build -ldflags '-w -s "-extldflags=-lresolv -L/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/usr/lib -F/Library/Developer/CommandLineTools/SDKs/MacOSX14.0.sdk/System/Library/Frameworks/" '


(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

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



;;; company
(yas-global-mode)
(global-company-mode t)
;; Navigate in completion minibuffer with `C-n` and `C-p`.
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
;; Provide instant autocompletion.
(setq company-idle-delay 0.0)


;; face
;; Or:
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

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
       (setenv "GOROOT" "/usr/local/go/")
       (setenv "GOPATH" "/Users/yayu/Golang")

       (add-to-list 'exec-path "~/Golang/bin")
       (if (file-exists-p "~/.go.env")
	   (load-env-vars "~/.go.env"))
       (setenv "PATH" (concat  "/usr/local/go/bin" ":" (getenv "PATH")))
       ))



;; go-mode lsp
(add-hook 'go-mode-hook #'lsp-deferred)

;; go mode test
(global-set-key (kbd "C-x g t") 'go-test-current-test)
(global-set-key (kbd "C-x g f") 'go-test-current-file)

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


;; flycheck
;; spell check
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.25)))


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
 '(column-number-mode t)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes '(tsdh-dark))
 '(custom-safe-themes
   '("e452b385e3f9cb05603fef58f3d65f73774137943fd75e6281dab3ff385851b9" "fdaf036ac62069f9b785ad2486b8106fb704b7c898d73ff7f66dc657523349d3" "fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0" default))
 '(git-gutter:update-interval 2)
 '(global-display-line-numbers-mode t)
 '(lsp-ui-imenu-auto-refresh 'after-save)
 '(lsp-ui-imenu-buffer-position 'left)
 '(package-selected-packages
   '(spacegray-theme melancholy-theme visual-replace markdownfmt mistty smart-mode-line git-gutter smart-compile rainbow-delimiters smartparens indent-bars dashboard blamer dockerfile-mode helm-ag dired-sidebar treemacs yaml-mode gotest symbol-overlay highlight-symbol imenu-list yasnippet ag flycheck company go-mode exec-path-from-shell helm))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UbuntuMono NF" :foundry "nil" :slant normal :weight regular :height 180 :width normal)))))




;; query-and-repalce function

(defun entire-buffer-replace (from to)
  "Do search and replace on entire buffer without moving point."
  (interactive "MReplace: \nMWith: ")
  (save-excursion
    (beginning-of-buffer)
    (let ((case-fold-search nil))
      (while (search-forward from nil t)
        (replace-match to t t)))))

(global-set-key (kbd "s-%") 'entire-buffer-replace)
(visual-replace-global-mode 1)
(global-set-key (kbd "C-c r") 'visual-replace)

;; ag search
;; first one create new window on the right. helm on the bottom, so we use helm.
(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)
(global-set-key (kbd "C-c s") 'ag-project-regexp)
;; (global-set-key (kbd "C-c s") 'helm-grep-do-git-grep)

;; mac dired ls
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; dired sidebar
(global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)



;; imenu buffer. this global key replaced by lsp-ui
;; (global-set-key (kbd "C-'") #'imenu-list-smart-toggle) 
(setq imenu-list-auto-resize t)
(setq imenu-list-position 'left)


;; symbol
(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
(global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)


;; windows
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer)


(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

	       

;; column number
(setq column-number-mode t)


;; flyspell

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
	(if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	  ;; else
	  (progn
	    (message "Flyspell on (text)")
	    (flyspell-mode 1)))
	;; I tried putting (flyspell-buffer) here but it didn't seem to work
	)))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
	(message "Flyspell off")
	(flyspell-mode -1))
					; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))
(global-set-key (kbd "C-c f") 'flyspell-toggle )


(defun my-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


;; supper key
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper


;; git blamer / git gutter
(global-set-key (kbd "C-c i") 'blamer-show-commit-info)
(global-git-gutter-mode +1)

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
;; (setq dashboard-items '((hackernews . 10)))

;; bookmarks
(global-set-key (kbd "C-x r l") 'helm-bookmarks)




;; indent-bar
;; (add-hook 'python-mode-hook #'indent-bars-mode)
;; (add-hook 'go-mode-hook #'indent-bars-mode)


;; finally, smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



;; smart compile
(defvar smart-compile-alist
  '(("\\.c$"          . "gcc -O2 %f -lm -o %n")
    ("\\.[Cc]+[Pp]*$" . "g++ -O2 %f -lm -o %n")
    ("\\.java$"       . "javac %f")
    ("\\.f90$"        . "f90 %f -o %n")
    ("go.mod$" . "go mod tidy")
    ("\\.go$" . "go run %f")
    ("\\.[Ff]$"       . "f77 %f -o %n")
    ("\\.pl$"         . "perl -cw %f")
    ("\\.mp$"	      . "mptopdf %f")
    ("\\.php$"        . "php %f")
    ("\\.tex$"        . "latex %f")
    ("\\.texi$"       . "makeinfo %f")
    ("\\.yaml$" . "kubectl apply -f %f")
    (emacs-lisp-mode  . (emacs-lisp-byte-compile))))

(global-set-key (kbd "<f6>") 'smart-compile)

;; mode line
(telephone-line-mode 1)


;; auto-revert
(setq global-auto-revert-mode t)


;; auto save  files
(setq auto-save-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs-saves/")))


;; theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "/lisp/"))
(load-theme 'miasma t)


;; set mark
(global-set-key (kbd "<s-SPC>") 'set-mark-command)


;; markdownfmt
(define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)
(put 'list-timers 'disabled nil)
