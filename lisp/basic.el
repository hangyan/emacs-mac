;;; package --- Summary:
;;; Commentary:
;;; Code:
;; line operation
(defun copy-current-line ()
  "Copy the current line without deleting it and highlight the line."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (copy-region-as-kill beg end)  ; Copy the line without deleting
    (activate-mark)                ; Highlight the region
    (message "Line copied"))
  (deactivate-mark)) ; Optionally deactivate the mark after operation

;; link: https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(use-package delsel
  :ensure
  nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

;; make C-g more useful
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; vertical minibuffer promt / fuzzy match
;; these packages will replace helm.
(use-package vertico
  :ensure
  t
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure
  t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure
  t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrride nil))

(use-package savehist
  :ensure
  nil ; it is built-in
  :hook (after-init . savehist-mode))




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



;; full path
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


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


(visual-replace-global-mode 1)


;;  open-line with indent
(defun my-open-line-and-indent (n)
  "Like `newline-and-indent' for the `open-line' command."
  (interactive "*p")
  (let ((eol (copy-marker (line-end-position))))
    (open-line n)
    (indent-region (point) eol)
    (set-marker eol nil)))

;; symbol
(require 'symbol-overlay)


;; auto-revert
(setq global-auto-revert-mode t)


;; auto save  files
(setq auto-save-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs-saves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs-saves/")))


;; dired imrpovement
(use-package dired
  :ensure
  nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure
  t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure
  t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))





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
       (add-to-list 'exec-path "c:/Program Files/Git/bin")
       (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin")
       (add-to-list 'exec-path
		    "c:/Users/win/AppData/Local/Microsoft/WindowsApps")
       (setenv "GOROOT" "c:/Program Files/Go")
       (setenv "GOPATH" "d:/go")
       (add-to-list 'exec-path "c:/Program Files/go/bin")
       (add-to-list 'exec-path "d:/go/bin")
       (setenv "PATH" (mapconcat #'identity exec-path path-separator)))
      ((eq system-type 'darwin)
       ;; mac-specific code goes here.
       (setenv "GOROOT" "/usr/local/Cellar/go/1.24.1/libexec/")
       (setenv "GOPATH" "/Users/yayu/Golang")
       (add-to-list 'exec-path "~/Golang/bin")
       (if (file-exists-p "~/.go.env")
	   (load-env-vars "~/.go.env"))
       (setenv "PATH"
	       (concat  "/usr/local/Cellar/go/1.24.1/libexec/bin" ":"
			(getenv "PATH")))
       ))


;; expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; ido jump --> no keys yet..
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.
SYMBOL-LIST."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching
	     t))
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

;; buffer cleanup
(use-package buffer-terminator
  :ensure
  t
  :custom
  (buffer-terminator-verbose nil)
  (buffer-terminator-verbose t)
  :config
  (buffer-terminator-mode 1))


;; a basic one. the helm/project one not working.
(defun find-files-in-git-project ()
  "Recursively search for and open files in the current Git project.
excluding files in .git directories."
  (interactive)
  (let ((project-root (vc-git-root default-directory)))
    (if project-root
        (let*
	    ((files (directory-files-recursively project-root "" nil))
             (filtered-files (seq-remove (lambda (file)
                                           (string-match-p
					    (concat "/"
						    (regexp-quote
						     ".git")
						    "/")
					    file))
                                         files))
             (relative-files (mapcar (lambda (file)
                                       (file-relative-name file
							   project-root))
                                     filtered-files))
             (file
	      (completing-read "Find file in project: "
			       relative-files)))
          (find-file (concat project-root file)))
      (message "Not inside a Git repository"))))

(global-set-key (kbd "C-c p f") 'find-files-in-git-project)
;; to avoid old habbits.
(global-set-key (kbd "C-x p f") 'find-files-in-git-project)


;; auto format for elisp

(defun my-elisp-format-buffer ()
  "Re-indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-elisp-format-buffer nil t)))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'basic)
;;; basic.el ends here
