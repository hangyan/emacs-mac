;;; early-init.el --- Early init -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;;; Code:


(setq use-package-compute-statistics t)


;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(setq gc-cons-threshold most-positive-fixnum
      ;; package-quickstart t
      file-name-handler-alist nil
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; Supress warnings
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)


;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(provide 'early-init)
;;; early-init.el ends here
