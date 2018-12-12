;; Use M-i when in isearch to jump to helm-swoop
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-mode-fuzzy-match t)

;; (defun helm-keyboard-quit ()
;;   "Quit minibuffer in helm.
;; If action buffer is displayed, kill it."
;;   (interactive)
;;   (with-helm-alive-p
;;     (when (get-buffer-window helm-action-buffer 'visible)
;;       (kill-buffer helm-action-buffer))
;;     (setq helm-exit-status 1)
;;     (condition-case nil (abort-recursive-edit) (error nil))))

(provide 'fd-helm)
