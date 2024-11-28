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

(global-set-key "\C-co" 'switch-to-minibuffer)


(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

;; column number
(setq column-number-mode t)

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'gui)
;;; gui.el ends here

