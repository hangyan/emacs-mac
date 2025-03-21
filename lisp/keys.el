;;; keys.el -- my all shortcut
;;; Commentary:
;;; Code:




;; supper key
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; windows
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
(global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)


;; set mark
(global-set-key (kbd "<s-SPC>") 'set-mark-command)
;; recentf
(global-set-key (kbd "s-b") 'recentf-open)
(global-set-key (kbd "s-%") 'entire-buffer-replace)

;; undo, fuck i never want to minilize my window
(global-set-key (kbd "C-z") 'undo)
(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
(global-set-key (kbd "C-o") 'my-open-line-and-indent)
(global-set-key (kbd "C-'") 'lsp-treemacs-symbols)

(global-set-key (kbd "<f6>") 'smart-compile)
(global-set-key (kbd "<f5>") 'treemacs)



;; git blamer / git gutter
(global-set-key (kbd "C-c i") 'blamer-show-commit-info)
(global-set-key "\C-co" 'switch-to-minibuffer)
;; ag search
(cond
 ((eq system-type 'windows-nt)
  (global-set-key (kbd "C-c s") 'helm-grep-do-git-grep))
 ((eq system-type 'darwin) ; macOS
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  (global-set-key (kbd "C-c s") 'ag-project-regexp)))

(global-set-key (kbd "C-c r") 'visual-replace)
(global-set-key (kbd "C-c i") 'helm-imenu)
;; kubernetes
(keymap-global-set "C-c k" 'kubed-prefix-map)
;; (kubed-menu-bar-mode)


;; go mode test
(global-set-key (kbd "C-x g t") 'go-test-current-test)
(global-set-key (kbd "C-x g f") 'go-test-current-file)

(global-set-key (kbd "C-c c") 'copy-current-line)
(global-set-key (kbd "C-x n c") 'backward-copy-word)
;; bookmarks
(global-set-key (kbd "C-x r l") 'list-bookmarks)


;; dired sidebar
(global-set-key (kbd "C-x C-n") 'dired-sidebar-toggle-sidebar)


(provide 'keys)
;;; keys.el ends here
