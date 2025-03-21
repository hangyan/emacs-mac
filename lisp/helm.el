;;; helm.el --- helm related config
;;; Commentary:
;;; Code:

;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package helm-config
;;   :ensure t
;;   :init
;;   (custom-set-variables '(helm-command-prefix-key "C-;"))
;;   :config
;;   (bind-keys :map helm-command-map
;;              ("a" . helm-ag)
;;              ("o" . helm-occur)
;;              ("y" . yas-insert-snippet)))

(require 'diminish)

(use-package helm
  ;; :init
  :ensure
  t
  :config
  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-quick-update t ; do not display invisible candidates
   helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
   helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
   helm-split-window-default-side 'other ;; open helm buffer in another window
   helm-split-window-inside-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-candidate-number-limit 200 ; limit the number of displayed canidates
   helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
   ;; helm-command
   helm-M-x-requires-pattern 0     ; show all candidates when set to 0
   )
  (bind-keys ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ("C-x b" . helm-mini)
	     ("C-x C-f". helm-find-files))
  (bind-keys :map helm-map
             ("C-o" . nil)
             ("TAB" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)
             ("C-h" . delete-backward-char)))



;; WTF. this is useful!!!
(use-package helm-swoop
  :ensure
  t
  :bind
  (("M-o" . helm-swoop)
   ("M-O" . helm-swoop-back-to-last-point)
   ("C-c M-o" . helm-multi-swoop)
   ;; ("C-c M-O" . helm-multi-swoop-all)
   )
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  (bind-keys :map isearch-mode-map
             ("M-o" . helm-swoop-from-isearch))
  (bind-keys :map helm-swoop-map
             ("M-o" . helm-multi-swoop-all-from-helm-swoop)
             ;; ("M-i" . helm-swoop-from-evil-search)
             )
  )

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook
	  'helm-save-current-pos-to-mark-ring)

;; ignore some buffers during switch; we should not skip the Warning buffer
;; as we need to fix the errors.
(setq helm-boring-buffer-regexp-list
      '("\\`\\*helm"        ; Ignore Helm’s own buffers
        "\\`\\*Echo"       ; Ignore echo area buffers
        "\\`\\*Minibuf"    ; Ignore minibuffer-related buffers
        "\\`\\*dashboard"  ; Ignore *Messages* buffer
	"\\`\\*ag search"  ; ag search
	"\\`\\*xref"       ; ignore xref/lsp
	"\\`\\*gopls"      ; lsp related
	"\\`\\*lsp-log"    ; lsp log
	"\\`\\*pylsp"      ; pylsp buffer
	"\\`\\*Bookmark"   ; bookmark list
	"\\`\\*Backtrace"  ; backtrace
	"\\`\\*LSP"        ; LSP errors
	"\\`\\*Flycheck"   ; flycheck errors
	"\\`\\*Async-native-com" ; native comp
        "\\`\\*scratch"    ; Ignore *scratch* buffer
	"\\`\\*Compile-Log"      ; compile log
        "\\` "             ; Ignore buffers starting with a space (internal)
        "\\`\\*Help"       ; Ignore *Help* buffer
	"\\`\\*Buffer List*"   ; buffer list buffer
        "\\`\\*Completions" ; Ignore completion buffers
	)
      )


;; start helm-mode
(use-package helm-mode
  :config
  (diminish 'helm-mode "")
  (helm-mode 1))


;; helm find files in project. not sure why the locate one is not working
;; and also project-find-files not working
;; (setq helm-locate-command "mdfind -name")


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:


;;; helm.el ends here
