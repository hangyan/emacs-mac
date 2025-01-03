;;; init.el --- my init file config:
;;; Commentary:
;;  my init file.  this file should load other configs first,
;;  then config language settings.
;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp/")

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
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; disable warning logs during package install
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))


(load-file "~/.emacs.d/lisp/basic.el")
(load-file "~/.emacs.d/lisp/gui.el")

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
(load-file "~/.emacs.d/lisp/yas.el")


;; face
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; go mode test
(global-set-key (kbd "C-x g t") 'go-test-current-test)
(global-set-key (kbd "C-x g f") 'go-test-current-file)

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
(global-set-key (kbd "s-b") 'recentf-open)

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

;; markdownfmt
(define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)
(put 'list-timers 'disabled nil)

;; ido imenu
(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "C-c i") 'ido-goto-symbol)


;; ido
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)
(setq ido-everywhere t)
;; (ido-mode t)

;; yes or now questions
(fset 'yes-or-no-p 'y-or-n-p)  ;; Ask for y/n instead of yes/no


;; undo, fuck i never want to minilize my window
(global-set-key (kbd "C-z") 'undo)

;; kubernetes
(keymap-global-set "C-c k" 'kubed-prefix-map)
;; (kubed-menu-bar-mode)


;; github copiot
;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main"))
;; (add-hook 'prog-mode-hook 'copilot-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
