;;; package --- Summary:
;;; Commentary:
;;; Code:
;; imenu buffer. this global key replaced by lsp-ui
;; (global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-auto-resize t)
(setq imenu-list-position 'left)

;; windows
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


;; column number
(setq column-number-mode t)

(cond
 ((eq system-type 'darwin) ; macOS
  (global-git-gutter-mode +1)))

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)


;; imenu
(add-hook 'markdown-mode-hook (lambda () (setq-local imenu-auto-rescan t)))
(add-hook 'makefile-mode-hook (lambda () (setq-local imenu-auto-rescan t)))
(add-hook 'prog-mode-hook
      (lambda ()
        (setq-local imenu-auto-rescan t)
        (setq-local imenu-sort-function #'imenu--sort-by-name)))


;; set font here to avoid os custom crap
;; set default font
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas-14" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Intel One Mono" (font-family-list))
    (set-frame-font "Intel One Mono" t t))))

;; set fond for mode line.
(set-face-attribute 'mode-line nil
                    :font "Intel One Mono")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'gui)
;;; gui.el ends here

