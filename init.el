;;; init.el --- my init file config:
;;; Commentary:
;;  my init file.  this file should load other configs first,
;;  then config language settings.
;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/3rd/")

;; for windows to use, set $HOME env for user first
(setq custom-file
      (expand-file-name (concat "~/.emacs.d/"
                                (if (eq system-type 'windows-nt)
                                    "win-"
                                  "")
                                "custom.el")))
(load custom-file :no-error-if-file-is-missing)

;; use package settings
(setq use-package-enable-imenu-support t)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;; disable warning logs during package install
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))


(load-file "~/.emacs.d/lisp/basic.el")
(load-file "~/.emacs.d/lisp/gui.el")
;; Does this slow down my emacs?
;; (load-file "~/.emacs.d/lisp/tabline.el")

(cond
 ((eq system-type 'darwin)
  (progn
    (load-file "~/.emacs.d/lisp/lsp.el")  ))
 (t
  (progn
    (message "skip loadding lsp for now."))))

;; does it kill lsp buffers?
					;(load-file "~/.emacs.d/lisp/auto-kill-buffers.el")
(load-file "~/.emacs.d/lisp/spell.el")
;; for performance.
;; (load-file "~/.emacs.d/lisp/yas.el")
(load-file "~/.emacs.d/lisp/python.el")
(load-file "~/.emacs.d/lisp/keys.el")
(load-file "~/.emacs.d/lisp/helm.el")

(message "load all files done")

;; face
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; flycheck
;; spell check
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(yaml-yamllint))))
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

(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")


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


;; markdownfmt
(define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)
(put 'list-timers 'disabled nil)


;; dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;; ido
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)
;; (setq ido-everywhere t)
;; (ido-mode t)
;; (ido-mode 'buffers)

;; yes or now questions
(fset 'yes-or-no-p 'y-or-n-p)  ;; Ask for y/n instead of yes/no


;; highlight current symbol
(use-package auto-highlight-symbol
					; this only installs it for programming mode derivatives; you can also make it global...
  :init (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :bind (:map auto-highlight-symbol-mode-map
              ("M-p" . ahs-backward)
              ("M-n" . ahs-forward)))

(load-file "~/.emacs.d/3rd/hl-todo.el")
;; highlight TODO...
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package rg
  :ensure t
  :config
  (rg-define-search my/rg-project
    "Search for any files in project or current directory"
    :query ask
    :format literal
    :confirm prefix
    :files "everything"
    :flags ("--hidden -g !.git")
    :dir (if (vc-root-dir)
             (vc-root-dir)
           default-directory))
  
  (rg-define-search my-rg-todo
    :query "(TODO|FIXME)"
    :format regexp
    :dir current
    :files current)
  :bind
  (("C-x p g" . rg-project)
   ("C-x p t" . my-rg-todo)
   ("M-SPC" . my/rg-project)))


(use-package emacs
  :config
  (defun meain/set-read-only-if-do-not-edit ()
    "Set the buffer to read-only if buffer contents has 'DO NOT EDIT' in it.
We limit the search to just top 10 lines so as to only check the header."
    (save-excursion
      (goto-char (point-min))
      (let ((content
             (buffer-substring (point)
                               (save-excursion (forward-line 10) (point)))))
        (when (and (not buffer-read-only)
                   (string-match "DO NOT EDIT" content))
          (read-only-mode 1)
          (message "Buffer seems to be generated. Set to read-only mode.")))))
  (add-hook 'find-file-hook 'meain/set-read-only-if-do-not-edit))


;; try to make emacs fast.
(setq display-line-numbers-type 'relative) ;; or 'visual for visual lines
(setq mac-mouse-wheel-smooth-scroll nil) ;; Avoid choppy scrolling
(setq mac-redisplay-dont-reset-vscroll t) ;; Avoid jumpy redisplay
(setq mac-mouse-wheel-mode t) ;; Improve mouse-wheel scrolling
(global-hl-line-mode -1)
;; redisplay_internal take a lots of CPU
;; (setq redisplay-dont-pause t) ;; no use any more
(setq redisplay-skip-fontification-on-input t)
(setq jit-lock-defer-time 0)     ;; Immediately update fontification
(setq jit-lock-stealth-time 1)   ;; More aggressive fontification delays
(setq jit-lock-contextually t)   ;; Context-sensitive fontification (for better performance)


;; debug on errors. maybe lead to quit.
(setq debug-on-error t)

(message "init done. happy coding.")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
