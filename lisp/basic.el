;;; package --- Summary:
;;; Commentary:
;;; Code:
;; line operation
(defun mark-from-point-to-end-of-line ()
  "Mark everything from point to end of line."
  (interactive)
  (set-mark (line-end-position))
  (activate-mark))

(global-set-key (kbd "C-x n f") 'mark-from-point-to-end-of-line)


(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq make-backup-files nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(defun backward-copy-word ()
  "Copy word backward."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-word) (point)))))
(global-set-key (kbd "C-x n c") 'backward-copy-word)

;; set mark
(global-set-key (kbd "<s-SPC>") 'set-mark-command)


;; full path
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq mac-command-modifier 'meta)


;; query-and-repalce function

(defun entire-buffer-replace (from to)
  "Do search and replace on entire buffer without moving point.
FROM mark point TO end."
  (interactive "MReplace: \nMWith: ")
  (save-excursion
    (goto-char (point-min))
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



;; symbol
(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
(global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)


;; auto-revert
(setq global-auto-revert-mode t)


;; auto save  files
(setq auto-save-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs-saves/")))


;; swith buffer improvment
(defun jos/switch-to-project-buffer ()
  "Run `switch-to-buffer' with the projects included as annotations."
  (interactive)
  (let ((completion-extra-properties
         '(:annotation-function (lambda (buffer)
                                  (concat
                                   " "
                                   (with-current-buffer buffer
                                     (if-let ((proj (project-current)))
                                         (propertize (project-root proj)
                                                     'face 'dired-directory)
                                       "<none>")))))))
    (call-interactively #'switch-to-buffer)))

(global-set-key (kbd "C-x b") 'jos/switch-to-project-buffer)




; path settings
(defun set-exec-path-from-shell-PATH ()
  "Path stuff."
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'basic)
;;; basic.el ends here
