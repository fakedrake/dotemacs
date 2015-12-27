(setq image-cache-eviction-delay 1)
(iimage-mode)

(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache t)
  (iimage-mode nil)
  (iimage-mode t))

(add-to-list 'compilation-finish-functions
             (lambda (buffer msg)
               (save-excursion
                 (set-buffer buffer)
                 (refresh-iimages))))

(provide 'fd-image)
