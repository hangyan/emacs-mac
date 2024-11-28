;;; package --- Summary:
;;; Commentary: my init file
;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp/")


(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


(load-file "~/.emacs.d/lisp/basic.el")
(load-file "~/.emacs.d/lisp/gui.el")
(load-file "~/.emacs.d/lisp/lsp.el")
(load-file "~/.emacs.d/lisp/auto-kill-buffers.el")
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
   '(popup yasnippet-snippets outline-indent spacegray-theme melancholy-theme visual-replace markdownfmt mistty smart-mode-line git-gutter smart-compile rainbow-delimiters smartparens indent-bars dashboard blamer dockerfile-mode helm-ag dired-sidebar treemacs yaml-mode gotest symbol-overlay highlight-symbol imenu-list yasnippet ag flycheck company go-mode exec-path-from-shell helm))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "UbuntuMono NF" :foundry "nil" :slant normal :weight regular :height 180 :width normal)))))


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


;; markdownfmt
(define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)
(put 'list-timers 'disabled nil)


;; outline
(setq outline-indent-ellipsis " ▼ ")
;; Python
(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

;; YAML
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
(add-hook 'go-mode-hook #'outline-indent-minor-mode)


(provide 'init)
;;; init.el ends here
