
;; RECENT FILES
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 100)
(defun steve-ido-choose-from-recentf ()
  "Select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)

(provide 'fd-recentfiles)
