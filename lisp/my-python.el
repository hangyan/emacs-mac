;;; python --- python setup:
;;; Commentary:
;;; Code:

;; python
(add-hook 'python-mode-hook #'lsp-deferred)

(use-package python-black 
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(setq python-indent-guess-indent-offset-verbose nil)


(setq python-black-extra-args "--skip-string-normalization")

(provide 'my-python)
;;; python.el ends here
