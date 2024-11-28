;;; package --- Summary:
;;; Commentary:
;;; Code:
(setq skeez/kill-unrecent-buffers-expire-secs 1800) ; 60s * 30min = 1800s

(defun skeez/kill-unrecent-buffers ()
  "Kill unrecent buffers."
  (let* ( (now (current-time))
	  (ts (format-time-string "%Y-%m-%d %T" now))
	  delta
	  )

    (dolist (buf (buffer-list))

      (when (buffer-live-p buf)

        (setq bts (with-current-buffer buf buffer-display-time)
              bn (buffer-name buf)
              since (if bts (round (float-time (time-subtract now bts))) 0)
              visible (get-buffer-window buf)
              special (or (string-prefix-p "*" bn) (string-prefix-p " *" bn))
              )

	(message "Inspecting buffer '%s' (%s) last shown %s (%s secs ago) .. visible %s special %s" buf bn bts since visible special)
	(if (and (> since skeez/kill-unrecent-buffers-expire-secs)
		 (buffer-file-name)
		 (not visible)
		 (not special) )
	    (progn (message "> Kill inactive file-buffer %s" bn)
		   (kill-buffer buf))
	  )

	) ; when
      ) ; dolist
    ) ; let

  )

;; (skeez/kill-unrecent-buffers)
(run-at-time 120 1200 'skeez/kill-unrecent-buffers)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'auto-kill-buffers)
;;; auto-kill-buffers.el ends here
